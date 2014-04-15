/*
Isaac Julien
Simulator for Wolf-Sheep herding
*/

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

import java.io.*;
import java.util.Scanner;


public class Simulator extends JPanel {

	// Types (wolf or sheep) and positions:
	private double[][][] positions;
	private int numWolves;
	private int numSheep;
	private int T;

	// Coordinates of field, clockwise from bottom right:
	private double[][] fieldCoords;
	// Coordinates of pen, clockwise from bottom right:
	private double[][] penCoords;




	private JTextArea readMsg;
	private JTextArea displayContents;
	private boolean readData;

	private JPanel buttonsPanel;
	DisplayPanel display;

	private Timer timer;
	private int milliseconds = 100;

	private static final long serialVersionUID = 1L;
	private static int FRAME_WIDTH=1100, FRAME_HEIGHT=600;
	private JFrame frame;
	
	

	////////////////////////////////////////// Setup /////////////////////////////////////////////////////

	public Simulator() {

		// Basic setup:
		frame = new JFrame("Herding Simulator v1.1");
		this.setOpaque(true);
		frame.setContentPane(this);
		frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
		frame.setSize(new Dimension(FRAME_WIDTH, FRAME_HEIGHT));

		// Keep track of if there's stuff to visualize yet:
		readData = false;

		// Layout:
		frame.setLayout(new BorderLayout());

		buttonsPanel = new JPanel();
		buttonsPanel.setPreferredSize(new Dimension(FRAME_WIDTH/4, FRAME_HEIGHT/2));
		add(buttonsPanel, BorderLayout.WEST);

		ConfigureFileReader();
		ConfigureDisplay();

		frame.setVisible(true);
	}


	////////////////////////////////////////// Display /////////////////////////////////////////////////////

	public void ConfigureDisplay() {

		display = new DisplayPanel();

		JButton runButton = new JButton("Run Simulation");
		buttonsPanel.add(runButton);

		runButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (!readData) return;
				display.updateDisplay();
			}
		});
		
		/* Timer sends signals to display: */
		final ActionListener timerListener = new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (!readData) return;
				display.updateDisplay();
			}
		};
		timer = new Timer(milliseconds, timerListener);
		
		/* Play button: */
		JButton playButton = new JButton("Run");
		buttonsPanel.add(playButton);
		playButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				timer.start();
			}
		});
		
		/* Pause button: */
		JButton pauseButton = new JButton("Pause");
		buttonsPanel.add(pauseButton);
		pauseButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				timer.stop();
			}
		});
		
		/* Pause button: */
		JButton restartButton = new JButton("Restart");
		buttonsPanel.add(restartButton);
		restartButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				timer.stop();
				display.resetT();
			}
		});
		
		
		/* Scroll bar to control playback speed: */
		final JScrollBar speedBar = new JScrollBar(JScrollBar.HORIZONTAL);
		speedBar.setMinimum(1);
		speedBar.setMaximum(1000);
		speedBar.setValue(milliseconds);
		buttonsPanel.add(speedBar);
		speedBar.addAdjustmentListener(new AdjustmentListener() {
			public void adjustmentValueChanged(AdjustmentEvent e) {
				milliseconds = speedBar.getValue();
				timer.stop();
				timer = new Timer(milliseconds, timerListener);
				
			}
		});


		add(display, BorderLayout.CENTER);

	}


	////////////////////////////////////////// Reading Files ///////////////////////////////////////////////////


	// Sets up the file reading portion:
	public void ConfigureFileReader() {
		final File currentDirectory = new File(".");
		JButton fileChooser = new JButton("Load file");
		fileChooser.addActionListener( new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				final JFileChooser fc = new JFileChooser(currentDirectory);
		        int returnVal = fc.showOpenDialog(null);
		        if (returnVal == JFileChooser.APPROVE_OPTION) {
		            File file = fc.getSelectedFile();
		            try {
		            	readFile(file);
		            	readMsg.setText("File read succesfully.");
		            	display.initializeDisplay(numWolves, numSheep, fieldCoords, penCoords, positions);
		            }
		            catch (FileNotFoundException exc) {
		            	readMsg.setText("File not found.");
		            }
		            catch (Exception exc) {
		            	readMsg.setText("Something went wrong.");
		            }
		        }
		        else return;
			}
		});

		buttonsPanel.add(fileChooser, BorderLayout.WEST);

		// Add message:
		readMsg = new JTextArea();
		readMsg.setEditable(false);
		add(readMsg, BorderLayout.NORTH);

		// Add display area:
		
		displayContents = new JTextArea();
		displayContents.setEditable(false);
		JScrollPane scrollContents = new JScrollPane(displayContents);
		JPanel textPanel = new JPanel();
		scrollContents.setPreferredSize(new Dimension(FRAME_WIDTH/4, FRAME_HEIGHT));
		textPanel.setPreferredSize(new Dimension(FRAME_WIDTH/4, FRAME_HEIGHT));
		textPanel.add(scrollContents);
		add(textPanel, BorderLayout.EAST);


	}

	public void readFile(File f) throws FileNotFoundException {

		fieldCoords = new double[4][2];
		penCoords = new double[4][2];

		Scanner s = new Scanner(f);

		String[] arr1 = s.nextLine().split(" ");
		String[] arr2 = s.nextLine().split(" ");
		for (int i=0; i<4; i++) {
			fieldCoords[i][0] = Double.parseDouble(arr1[2*i]);
			fieldCoords[i][1] = Double.parseDouble(arr1[2*i+1]);
			penCoords[i][0] = Double.parseDouble(arr2[2*i]);
			penCoords[i][1] = Double.parseDouble(arr2[2*i+1]);
		}

		String[] arr3 = s.nextLine().split(" ");
		numWolves = Integer.parseInt(arr3[0]);
		numSheep = Integer.parseInt(arr3[1]);
		T = Integer.parseInt(arr3[2]);

		positions = new double[numWolves+numSheep][T][2];

		int t = 0;
		while (s.hasNextLine()) {
			String[] temp = s.nextLine().split(" ");
			for (int i=0; i < numWolves + numSheep; i++) {
				positions[i][t][0] = Double.parseDouble(temp[2*i]);
				positions[i][t][1] = Double.parseDouble(temp[2*i+1]);
			}
			t++;
		}

		// Set display pane:
		String disp = "";
		disp += "Number of wolves: " + numWolves + "\n";
		disp += "Number of sheep: " + numSheep + "\n";
		disp += "\n-------------------------------\n";
		for (t=0; t < T; t++) {
			for (int i=0; i < numWolves + numSheep; i++) {
					disp += "("+positions[i][t][0]+","+positions[i][t][1]+")\t";
			}
			disp += "\n";
		}
		displayContents.setText(disp);
		readData = true;
	}
	
	public static void main(String[] args) {
		javax.swing.SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				new Simulator();
			}
		});
	}


}