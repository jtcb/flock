/*
Isaac Julien
Simulator for Wolf-Sheep herding
*/

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

import java.io.*;
import java.util.*;

import java.awt.geom.*;

import javax.imageio.ImageIO;


public class DisplayPanel extends JPanel {

	private static int FRAME_WIDTH=300, FRAME_HEIGHT=300;

	private boolean init = false;
	private double[][] field, pen;
	private int numWolves, numSheep;
	private double[][][] positions;
	private int t;


	double scale = 5;
	double x0 = 50, y0 = 50;


	Image sheep;
	Image wolf;
	Image arrow;
	{
		try{ sheep = ImageIO.read(new File("sheep.jpg")); } catch(IOException e) {}
		try{ wolf = ImageIO.read(new File("wolf.jpg")); } catch(IOException e) {}
	}


	public DisplayPanel() {
        setBorder(BorderFactory.createLineBorder(Color.black));
        setPreferredSize(new Dimension(FRAME_WIDTH, FRAME_HEIGHT));
    }

    public void paintComponent(Graphics g) {


        super.paintComponent(g);

        Graphics2D g2 = (Graphics2D) g;
        g2.setPaint(Color.BLACK);

        if (init) {

        	double width = field[0][0] - field[1][0];
        	double height = field[3][1] - field[0][1];
			Rectangle2D.Double rect = new Rectangle2D.Double(x0, y0, width*scale, height*scale);
			g2.draw(rect);


			width = pen[0][0] - pen[1][0];
        	height = pen[3][1] - pen[0][1];
			rect = new Rectangle2D.Double(x0 + pen[1][0], y0 + pen[1][1], width*scale, height*scale);
			g2.setPaint(Color.GRAY);
			g2.fill(rect);

			g2.setPaint(Color.BLACK);
        }

        if (init && t >= 0) {
			// Draw wolves and sheep:
        	for (int i=0; i<numWolves; i++) {
				double x = positions[i][t][0];
				double y = positions[i][t][1];
				AffineTransform transform = new AffineTransform(1, 0, 0, 1, x*scale + x0, y*scale + y0);
				g2.drawImage(wolf, transform, null);
			}
			for (int i=numWolves; i<numWolves+numSheep; i++) {
				double x = positions[i][t][0];
				double y = positions[i][t][1];
				AffineTransform transform = new AffineTransform(1, 0, 0, 1, x*scale + x0, y*scale + y0);
				g2.drawImage(sheep, transform, null);
			}

        }

        // Draw Text
        g.drawString("SIMULATION",20,20);
    }


	public void initializeDisplay(int numWolves, int numSheep, double[][] field, double[][] pen, double[][][] positions) {
		this.field = field;
		this.pen = pen;
		this.numWolves = numWolves;
		this.numSheep = numSheep;
		this.positions = positions;
		this.t = 0;
		init = true;
		repaint();
	}

	public void resetT() {
		t = 0;
		repaint();
	}


	public void updateDisplay() {
		t++;
		if (positions.length == 0 || t >= positions[0].length) return;
		repaint();
	}

}