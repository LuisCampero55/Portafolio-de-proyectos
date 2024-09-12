import com.sun.prism.paint.Color;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.util.Set;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JOptionPane;

public class Maquina extends javax.swing.JFrame {
    Icon Q0 = new ImageIcon(getClass().getResource("/imagenes/Q0.png"));
    Icon Q1 = new ImageIcon(getClass().getResource("/imagenes/Q1.png"));
    Icon Q2 = new ImageIcon(getClass().getResource("/imagenes/Q2.png"));
    Icon Q3 = new ImageIcon(getClass().getResource("/imagenes/Q3.png"));
    Icon Q4 = new ImageIcon(getClass().getResource("/imagenes/Q4.png"));
    Icon Q5 = new ImageIcon(getClass().getResource("/imagenes/Q5.png"));
    Icon Q6 = new ImageIcon(getClass().getResource("/imagenes/Q6.png"));
    Icon Q7 = new ImageIcon(getClass().getResource("/imagenes/Q7.png"));
    Icon Q8 = new ImageIcon(getClass().getResource("/imagenes/Q8.png"));
    Icon Q9 = new ImageIcon(getClass().getResource("/imagenes/Q9.png"));
    Icon Q10 = new ImageIcon(getClass().getResource("/imagenes/Q10.png"));
    Icon Q11 = new ImageIcon(getClass().getResource("/imagenes/Q11.png"));
    Icon Q12 = new ImageIcon(getClass().getResource("/imagenes/Q12.png"));
    Icon Q13 = new ImageIcon(getClass().getResource("/imagenes/Q13.png"));
    
    private int x1;
    private int y1;
    public String operador;
    public float din, cam;
    public int d1, d2, d3, d4, d5;
    Visualizacion trab = new Visualizacion();
    private Rectangle rect;
    
    public Maquina() {
        initComponents();
        
        rect = getBounds();
        Image image = createImage(rect.width,rect.height);
        Graphics gi = image.getGraphics();
        d1=0;
        d2=0;
        d3=0;
        d4=0;
        d5=0;
        din = 0;
        cam = 0;
        this.setLocationRelativeTo(null);
        paint(gi);
    }
    public void exe(int d1, int d2, int d3, int d4){
        ImageIcon cinco = new ImageIcon(getClass().getResource("/imagenes/5.png"));
        ImageIcon icinco = new ImageIcon(cinco.getImage().getScaledInstance(din1.getWidth(), din1.getHeight(), Image.SCALE_DEFAULT));
        if(d1==1){
            din1.setIcon(icinco);
            cam1.setText("x1");
        }
        ImageIcon diez = new ImageIcon(getClass().getResource("/imagenes/10.png"));
        ImageIcon idiez = new ImageIcon(diez.getImage().getScaledInstance(din2.getWidth(), din2.getHeight(), Image.SCALE_DEFAULT));
        if(d2==1){
            din2.setIcon(idiez);
            cam2.setText("x1");
        }
        if(d2==2){
            din2.setIcon(idiez);
            cam2.setText("x2");
        }
        ImageIcon veinte = new ImageIcon(getClass().getResource("/imagenes/20.png"));
        ImageIcon iveinte = new ImageIcon(veinte.getImage().getScaledInstance(din3.getWidth(), din3.getHeight(), Image.SCALE_DEFAULT));
        if(d3==1){
            din3.setIcon(iveinte);
            cam3.setText("x1");
        }
        ImageIcon cincuenta = new ImageIcon(getClass().getResource("/imagenes/50.png"));
        ImageIcon icincuenta = new ImageIcon(cincuenta.getImage().getScaledInstance(din4.getWidth(), din4.getHeight(), Image.SCALE_DEFAULT));
        if(d4==1){
            din4.setIcon(icincuenta);
            cam4.setText("x1");
        }
        ImageIcon cien = new ImageIcon(getClass().getResource("/imagenes/100.png"));
        ImageIcon icien = new ImageIcon(cien.getImage().getScaledInstance(din5.getWidth(), din5.getHeight(), Image.SCALE_DEFAULT));
        if(d5==1){
            din5.setIcon(icincuenta);
            cam5.setText("x1");
        }
    }
    public void cambio(float camb){
        if(cam == 0){
            d1=0;
            d2=0;
            d3=0;
            d4=0;
            exe(d1, d2, d3, d4);
            din = 0;
        }
        if(cam == 5){
            d1=1;
            d2=0;
            d3=0;
            d4=0;
            exe(d1, d2, d3, d4);
            din = 0;
        }
                if(cam == 10){
                    d1=0;
                    d2=1;
                    d3=0;
                    d4=0;
                    exe(d1, d2, d3, d4);
                    din = 0;
                }
                if(cam == 15){
                    d1=1;
                    d2=1;
                    d3=0;
                    d4=0;
                    exe(d1, d2, d3, d4);
                    din = 0;
                }
                if(cam == 20){
                    d1=0;
                    d2=0;
                    d3=1;
                    d4=0;
                    exe(d1, d2, d3, d4);
                    din = 0;
                }
                if(cam == 25){
                    d1=1;
                    d2=0;
                    d3=1;
                    d4=0;
                    exe(d1, d2, d3, d4);
                    din = 0;
                }
                if(cam == 30){
                    d1=0;
                    d2=1;
                    d3=1;
                    d4=0;
                    exe(d1, d2, d3, d4);
                    din = 0;
                }
                if(cam == 35){
                    d1=1;
                    d2=1;
                    d3=1;
                    d4=0;
                    exe(d1, d2, d3, d4);
                    din = 0;
                }
                if(cam == 40){
                    d1=0;
                    d2=0;
                    d3=2;
                    d4=0;
                    exe(d1, d2, d3, d4);
                    din = 0;
                }
                if(cam == 45){
                    d1=1;
                    d2=2;
                    d3=1;
                    d4=0;
                    exe(d1, d2, d3, d4);
                    din = 0;
                }
                if(cam == 50){
                    d1=0;
                    d2=0;
                    d3=0;
                    d4=1;
                    exe(d1, d2, d3, d4);
                    din = 0;
                }
                if(cam == 55){
                    d1=1;
                    d2=0;
                    d3=0;
                    d4=1;
                    exe(d1, d2, d3, d4);
                    din = 0;
                }
                if(cam == 60){
                    d1=0;
                    d2=1;
                    d3=0;
                    d4=1;
                    exe(d1, d2, d3, d4);
                    din = 0;
                }
                if(cam == 65){
                    d1=1;
                    d2=1;
                    d3=0;
                    d4=1;
                    exe(d1, d2, d3, d4);
                    din = 0;
                }
                if(cam == 70){
                    d1=0;
                    d2=0;
                    d3=1;
                    d4=1;
                    exe(d1, d2, d3, d4);
                    din = 0;
                }
                if(cam == 75){
                    d1=1;
                    d2=0;
                    d3=1;
                    d4=1;
                    exe(d1, d2, d3, d4);
                    din = 0;
                }
                if(cam == 80){
                    d1=0;
                    d2=1;
                    d3=1;
                    d4=1;
                    exe(d1, d2, d3, d4);
                    din = 0;
                }
                if(cam == 85){
                    d1=1;
                    d2=1;
                    d3=1;
                    d4=1;
                    exe(d1, d2, d3, d4);
                    din = 0;
                }
                if(cam == 90){
                    d1=0;
                    d2=0;
                    d3=2;
                    d4=1;
                    exe(d1, d2, d3, d4);
                    din = 0;
                }
    }
    public void limpiar(){
        din1.setIcon(null);
        cam1.setText("");
        din2.setIcon(null);
        cam2.setText("");
        din3.setIcon(null);
        cam3.setText("");
        din4.setIcon(null);
        cam4.setText("");
        din5.setIcon(null);
        cam5.setText("");
    }
    
    //Metodos para ingresar el dínero
    public void Cinco(){
        limpiar();
        din = din + 5;
        operador = "" + din;
        LDinero.setText(operador);
    }
    public void Diez(){
        limpiar();
        din = din + 10;
        operador = "" + din;
        LDinero.setText(operador);
        LCambio.setText("");
    }
    public void Veinte(){
        limpiar();
        din = din + 20;
        operador = "" + din;
        LDinero.setText(operador);
        LCambio.setText("");
    }
    public void Cincuenta(){
        limpiar();
        din = din + 50;
        operador = "" + din;
        LDinero.setText(operador);
        LCambio.setText("");
    }
    public void Cien(){
        limpiar();
        din = din + 100;
        operador = "" + din;
        LDinero.setText(operador);
        LCambio.setText("");
    }
    
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jInternalFrame1 = new javax.swing.JInternalFrame();
        jPanel3 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        cincobtn = new javax.swing.JButton();
        jLabel4 = new javax.swing.JLabel();
        LDinero = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        diezbtn = new javax.swing.JButton();
        cienbtn = new javax.swing.JButton();
        veintebtn = new javax.swing.JButton();
        cincuentabtn = new javax.swing.JButton();
        jPanel1 = new javax.swing.JPanel();
        seisRB = new javax.swing.JRadioButton();
        liRB = new javax.swing.JRadioButton();
        litrosRB = new javax.swing.JRadioButton();
        jLabel5 = new javax.swing.JLabel();
        jugo1 = new javax.swing.JLabel();
        jugo2 = new javax.swing.JLabel();
        jugo3 = new javax.swing.JLabel();
        ComprarBtn = new javax.swing.JButton();
        CancelarBtn = new javax.swing.JButton();
        LCambio = new javax.swing.JLabel();
        CambioBtn1 = new javax.swing.JButton();
        jPanel2 = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        din1 = new javax.swing.JLabel();
        din2 = new javax.swing.JLabel();
        din3 = new javax.swing.JLabel();
        din4 = new javax.swing.JLabel();
        din5 = new javax.swing.JLabel();
        cam1 = new javax.swing.JLabel();
        cam2 = new javax.swing.JLabel();
        cam3 = new javax.swing.JLabel();
        cam5 = new javax.swing.JLabel();
        cam4 = new javax.swing.JLabel();
        automatalabel = new javax.swing.JLabel();

        jInternalFrame1.setVisible(true);

        javax.swing.GroupLayout jInternalFrame1Layout = new javax.swing.GroupLayout(jInternalFrame1.getContentPane());
        jInternalFrame1.getContentPane().setLayout(jInternalFrame1Layout);
        jInternalFrame1Layout.setHorizontalGroup(
            jInternalFrame1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 0, Short.MAX_VALUE)
        );
        jInternalFrame1Layout.setVerticalGroup(
            jInternalFrame1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 0, Short.MAX_VALUE)
        );

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        setTitle("Vending Machine");

        jPanel3.setBackground(new java.awt.Color(204, 255, 255));
        jPanel3.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));

        jLabel1.setFont(new java.awt.Font("Arial Black", 3, 36)); // NOI18N
        jLabel1.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        jLabel1.setText("MAQUINA DE JUGOS ");

        cincobtn.setIcon(new javax.swing.ImageIcon(getClass().getResource("/imagenes/5.png"))); // NOI18N
        cincobtn.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
        cincobtn.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cincobtnActionPerformed(evt);
            }
        });

        jLabel4.setFont(new java.awt.Font("Arial", 1, 24)); // NOI18N
        jLabel4.setText("Dinero:");

        LDinero.setFont(new java.awt.Font("Tahoma", 0, 18)); // NOI18N

        jLabel3.setFont(new java.awt.Font("Arial", 1, 24)); // NOI18N
        jLabel3.setText("Su cambio es de:");

        diezbtn.setIcon(new javax.swing.ImageIcon(getClass().getResource("/imagenes/10.png"))); // NOI18N
        diezbtn.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
        diezbtn.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                diezbtnActionPerformed(evt);
            }
        });

        cienbtn.setIcon(new javax.swing.ImageIcon(getClass().getResource("/imagenes/100.png"))); // NOI18N
        cienbtn.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
        cienbtn.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cienbtnActionPerformed(evt);
            }
        });

        veintebtn.setIcon(new javax.swing.ImageIcon(getClass().getResource("/imagenes/20.png"))); // NOI18N
        veintebtn.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
        veintebtn.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                veintebtnActionPerformed(evt);
            }
        });

        cincuentabtn.setIcon(new javax.swing.ImageIcon(getClass().getResource("/imagenes/50.png"))); // NOI18N
        cincuentabtn.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
        cincuentabtn.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cincuentabtnActionPerformed(evt);
            }
        });

        jPanel1.setBackground(new java.awt.Color(204, 255, 255));
        jPanel1.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        jPanel1.setForeground(new java.awt.Color(204, 255, 255));

        seisRB.setBackground(new java.awt.Color(204, 255, 255));
        seisRB.setFont(new java.awt.Font("Arial", 1, 18)); // NOI18N
        seisRB.setText("600 ml  $15 ");
        seisRB.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                seisRBActionPerformed(evt);
            }
        });

        liRB.setBackground(new java.awt.Color(204, 255, 255));
        liRB.setFont(new java.awt.Font("Arial", 1, 18)); // NOI18N
        liRB.setText("1 litro $25 ");
        liRB.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                liRBActionPerformed(evt);
            }
        });

        litrosRB.setBackground(new java.awt.Color(204, 255, 255));
        litrosRB.setFont(new java.awt.Font("Arial", 1, 18)); // NOI18N
        litrosRB.setText("2 litros $45");
        litrosRB.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                litrosRBActionPerformed(evt);
            }
        });

        jLabel5.setFont(new java.awt.Font("Arial", 1, 24)); // NOI18N
        jLabel5.setText("Eliga el tamaño de jugo");

        jugo1.setIcon(new javax.swing.ImageIcon(getClass().getResource("/imagenes/600.jpg"))); // NOI18N

        jugo2.setIcon(new javax.swing.ImageIcon(getClass().getResource("/imagenes/litro.jpg"))); // NOI18N

        jugo3.setIcon(new javax.swing.ImageIcon(getClass().getResource("/imagenes/doslitro.jpg"))); // NOI18N

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jLabel5)
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(seisRB)
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jugo1, javax.swing.GroupLayout.PREFERRED_SIZE, 108, javax.swing.GroupLayout.PREFERRED_SIZE)))
                        .addGap(32, 32, 32)
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addComponent(jugo2, javax.swing.GroupLayout.PREFERRED_SIZE, 108, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addComponent(jugo3, javax.swing.GroupLayout.PREFERRED_SIZE, 108, javax.swing.GroupLayout.PREFERRED_SIZE))
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addComponent(liRB)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 32, Short.MAX_VALUE)
                                .addComponent(litrosRB)))))
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                .addComponent(jLabel5)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jugo1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jugo2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jugo3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(seisRB)
                    .addComponent(liRB)
                    .addComponent(litrosRB)))
        );

        ComprarBtn.setFont(new java.awt.Font("Arial", 1, 24)); // NOI18N
        ComprarBtn.setText("Comprar");
        ComprarBtn.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
        ComprarBtn.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                ComprarBtnActionPerformed(evt);
            }
        });

        CancelarBtn.setFont(new java.awt.Font("Arial", 1, 24)); // NOI18N
        CancelarBtn.setText("Cancelar");
        CancelarBtn.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
        CancelarBtn.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                CancelarBtnActionPerformed(evt);
            }
        });

        LCambio.setFont(new java.awt.Font("Tahoma", 0, 18)); // NOI18N

        CambioBtn1.setFont(new java.awt.Font("Arial", 1, 24)); // NOI18N
        CambioBtn1.setText("Cambio");
        CambioBtn1.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
        CambioBtn1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                CambioBtn1ActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(jLabel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addGroup(jPanel3Layout.createSequentialGroup()
                                    .addComponent(ComprarBtn)
                                    .addGap(61, 61, 61)
                                    .addComponent(CambioBtn1)
                                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(CancelarBtn)))
                            .addGroup(jPanel3Layout.createSequentialGroup()
                                .addGap(18, 18, 18)
                                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(jLabel4, javax.swing.GroupLayout.PREFERRED_SIZE, 132, javax.swing.GroupLayout.PREFERRED_SIZE)
                                    .addComponent(jLabel3))
                                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addGroup(jPanel3Layout.createSequentialGroup()
                                        .addGap(16, 16, 16)
                                        .addComponent(LDinero, javax.swing.GroupLayout.PREFERRED_SIZE, 140, javax.swing.GroupLayout.PREFERRED_SIZE))
                                    .addGroup(jPanel3Layout.createSequentialGroup()
                                        .addGap(18, 18, 18)
                                        .addComponent(LCambio, javax.swing.GroupLayout.PREFERRED_SIZE, 140, javax.swing.GroupLayout.PREFERRED_SIZE)))))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                            .addGroup(jPanel3Layout.createSequentialGroup()
                                .addComponent(diezbtn, javax.swing.GroupLayout.PREFERRED_SIZE, 0, Short.MAX_VALUE)
                                .addGap(18, 18, 18)
                                .addComponent(cincobtn, javax.swing.GroupLayout.PREFERRED_SIZE, 90, javax.swing.GroupLayout.PREFERRED_SIZE))
                            .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                .addComponent(cincuentabtn, javax.swing.GroupLayout.PREFERRED_SIZE, 198, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addComponent(cienbtn, javax.swing.GroupLayout.PREFERRED_SIZE, 198, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addComponent(veintebtn, javax.swing.GroupLayout.PREFERRED_SIZE, 199, javax.swing.GroupLayout.PREFERRED_SIZE)))
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addGap(5, 5, 5))
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addComponent(jLabel1, javax.swing.GroupLayout.PREFERRED_SIZE, 88, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 37, Short.MAX_VALUE)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(LDinero, javax.swing.GroupLayout.PREFERRED_SIZE, 25, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jLabel4))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                            .addComponent(jLabel3)
                            .addComponent(LCambio, javax.swing.GroupLayout.PREFERRED_SIZE, 25, javax.swing.GroupLayout.PREFERRED_SIZE)))
                    .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                        .addComponent(cincobtn, javax.swing.GroupLayout.PREFERRED_SIZE, 83, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addComponent(diezbtn, javax.swing.GroupLayout.PREFERRED_SIZE, 0, Short.MAX_VALUE)))
                .addGap(14, 14, 14)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                        .addComponent(veintebtn)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(cincuentabtn)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(cienbtn))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                        .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(ComprarBtn)
                            .addComponent(CancelarBtn)
                            .addComponent(CambioBtn1))))
                .addGap(19, 19, 19))
        );

        jPanel2.setBackground(new java.awt.Color(224, 228, 227));
        jPanel2.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));

        jLabel2.setFont(new java.awt.Font("Arial Black", 3, 36)); // NOI18N
        jLabel2.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        jLabel2.setText("CAMBIO");

        cam1.setFont(new java.awt.Font("Arial", 0, 24)); // NOI18N
        cam1.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);

        cam2.setFont(new java.awt.Font("Arial", 0, 24)); // NOI18N
        cam2.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);

        cam3.setFont(new java.awt.Font("Arial", 0, 24)); // NOI18N
        cam3.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);

        cam5.setFont(new java.awt.Font("Arial", 0, 24)); // NOI18N
        cam5.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);

        cam4.setFont(new java.awt.Font("Arial", 0, 24)); // NOI18N
        cam4.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(din1, javax.swing.GroupLayout.PREFERRED_SIZE, 100, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(cam1, javax.swing.GroupLayout.PREFERRED_SIZE, 100, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(din2, javax.swing.GroupLayout.PREFERRED_SIZE, 100, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(cam2, javax.swing.GroupLayout.PREFERRED_SIZE, 100, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(din3, javax.swing.GroupLayout.PREFERRED_SIZE, 205, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(cam3, javax.swing.GroupLayout.PREFERRED_SIZE, 205, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(cam4, javax.swing.GroupLayout.PREFERRED_SIZE, 205, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(cam5, javax.swing.GroupLayout.PREFERRED_SIZE, 13, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(din4, javax.swing.GroupLayout.PREFERRED_SIZE, 205, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(din5, javax.swing.GroupLayout.PREFERRED_SIZE, 13, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addGap(0, 0, Short.MAX_VALUE))
            .addComponent(jLabel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addComponent(jLabel2, javax.swing.GroupLayout.PREFERRED_SIZE, 88, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                        .addComponent(din4, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, 100, Short.MAX_VALUE)
                        .addComponent(din3, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(din2, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(din1, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    .addComponent(din5, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.PREFERRED_SIZE, 100, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(cam1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(cam2, javax.swing.GroupLayout.DEFAULT_SIZE, 38, Short.MAX_VALUE)
                    .addComponent(cam3, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(cam5, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(cam4, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );

        automatalabel.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        automatalabel.setIcon(new javax.swing.ImageIcon(getClass().getResource("/imagenes/Q0.png"))); // NOI18N
        automatalabel.setVerticalAlignment(javax.swing.SwingConstants.TOP);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(automatalabel, javax.swing.GroupLayout.PREFERRED_SIZE, 1316, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addComponent(jPanel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addGap(18, 18, 18)
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addGroup(layout.createSequentialGroup()
                .addGap(63, 63, 63)
                .addComponent(automatalabel, javax.swing.GroupLayout.PREFERRED_SIZE, 705, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void seisRBActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_seisRBActionPerformed
        if (seisRB.isSelected()){
            liRB.setSelected(false);
            litrosRB.setSelected(false);
            automatalabel.setIcon(Q1);
        }
    }//GEN-LAST:event_seisRBActionPerformed

    private void CancelarBtnActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_CancelarBtnActionPerformed
        automatalabel.setIcon(Q0);
        System.exit(0);
    }//GEN-LAST:event_CancelarBtnActionPerformed

    private void liRBActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_liRBActionPerformed
        if (liRB.isSelected()){
            seisRB.setSelected(false);
            litrosRB.setSelected(false);
            automatalabel.setIcon(Q2);
        }
    }//GEN-LAST:event_liRBActionPerformed

    private void litrosRBActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_litrosRBActionPerformed
        if (litrosRB.isSelected()){
            liRB.setSelected(false);
            seisRB.setSelected(false);
            automatalabel.setIcon(Q3);
        }
    }//GEN-LAST:event_litrosRBActionPerformed

    private void ComprarBtnActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_ComprarBtnActionPerformed
        Icon jugo1 = new ImageIcon(getClass().getResource("/imagenes/600.jpg"));
        Icon jugo2 = new ImageIcon(getClass().getResource("/imagenes/litro.jpg"));
        Icon jugo3 = new ImageIcon(getClass().getResource("/imagenes/doslitro.jpg"));
        if(seisRB.isSelected()){
            automatalabel.setIcon(Q4);
            cam = din - 15;
            if (cam < 0 ){
                JOptionPane.showMessageDialog(rootPane, "Lo siento no tienes suficiente dinero");
            }else{
                trab.setVisible(true);
                JOptionPane.showMessageDialog(rootPane, "Recoja su cambio");
                automatalabel.setIcon(Q8);
                trab.setVisible(false);
                JOptionPane.showMessageDialog(rootPane, "Cambio recogido");
                cambio(cam);
                automatalabel.setIcon(Q10);
                JOptionPane.showMessageDialog(null,"Recoja su jugo de 600 Mililitros", "Gracias por su compra", JOptionPane.PLAIN_MESSAGE, jugo1);
                automatalabel.setIcon(Q11);
                LCambio.setText(Double.toString(cam));
                LDinero.setText("");
                din = 0;
            }
        }else if(liRB.isSelected()){
            trab.setVisible(true);
            automatalabel.setIcon(Q5);
            cam = din - 25;
            if (cam < 0 ){
                JOptionPane.showMessageDialog(rootPane, "Lo siento no tienes suficiente dinero");
            }else{
                trab.setVisible(true);
                JOptionPane.showMessageDialog(rootPane, "Recoja su cambio");
                automatalabel.setIcon(Q8);
                trab.setVisible(false);
                JOptionPane.showMessageDialog(rootPane, "Cambio recogido");
                cambio(cam);
                automatalabel.setIcon(Q10);
                JOptionPane.showMessageDialog(null,"Recoja su jugo de 1 Litro", "Gracias por su compra", JOptionPane.PLAIN_MESSAGE, jugo2);
                automatalabel.setIcon(Q12);
                LCambio.setText(Double.toString(cam));
                LDinero.setText("");
                din = 0;
            }
        }else if(litrosRB.isSelected()){
            trab.setVisible(true);
            automatalabel.setIcon(Q6);
            cam = din - 45;
            if (cam < 0 ){
                JOptionPane.showMessageDialog(rootPane, "Lo siento no tienes suficiente dinero");
            }else{
                trab.setVisible(true);
                JOptionPane.showMessageDialog(rootPane, "Recoja su cambio");
                automatalabel.setIcon(Q8);
                trab.setVisible(false);
                JOptionPane.showMessageDialog(rootPane, "Cambio recogido");
                cambio(cam);
                automatalabel.setIcon(Q10);
                JOptionPane.showMessageDialog(null,"Recoja su jugo de 2 Litros", "Gracias por su compra", JOptionPane.PLAIN_MESSAGE, jugo3);
                automatalabel.setIcon(Q13);
                LCambio.setText(Double.toString(cam));
                LDinero.setText("");
                din = 0;
            }
        }
    }//GEN-LAST:event_ComprarBtnActionPerformed

    //Botones de Dinero
    private void diezbtnActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_diezbtnActionPerformed
        if(seisRB.isSelected()){
            automatalabel.setIcon(Q4);
            Diez();
        }else if(liRB.isSelected()){
            automatalabel.setIcon(Q5);
            Diez();
        }else if(litrosRB.isSelected()){
            automatalabel.setIcon(Q6);
            Diez();
        }else if(seisRB.isSelected() == false || liRB.isSelected() == false || litrosRB.isSelected() == false){
            JOptionPane.showMessageDialog(rootPane, "Porfavor escoja un tipo de jugo");
            automatalabel.setIcon(Q0);
        }
    }//GEN-LAST:event_diezbtnActionPerformed

    private void cincobtnActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cincobtnActionPerformed
        if(seisRB.isSelected()){
            automatalabel.setIcon(Q4);
            Cinco();
        }else if(liRB.isSelected()){
            automatalabel.setIcon(Q5);
            Cinco();
        }else if(litrosRB.isSelected()){
            automatalabel.setIcon(Q6);
            Cinco();
        }else if(seisRB.isSelected() == false || liRB.isSelected() == false || litrosRB.isSelected() == false){
            JOptionPane.showMessageDialog(rootPane, "Porfavor escoja un tipo de jugo");
            automatalabel.setIcon(Q0);
        }
    }//GEN-LAST:event_cincobtnActionPerformed

    private void veintebtnActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_veintebtnActionPerformed
        if(seisRB.isSelected()){
            automatalabel.setIcon(Q4);
            Veinte();
        }else if(liRB.isSelected()){
            automatalabel.setIcon(Q5);
            Veinte();
        }else if(litrosRB.isSelected()){
            automatalabel.setIcon(Q6);
            Veinte();
        }else if(seisRB.isSelected() == false || liRB.isSelected() == false || litrosRB.isSelected() == false){
            JOptionPane.showMessageDialog(rootPane, "Porfavor escoja un tipo de jugo");
            automatalabel.setIcon(Q0);
        }
    }//GEN-LAST:event_veintebtnActionPerformed

    private void cincuentabtnActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cincuentabtnActionPerformed
        if(seisRB.isSelected()){
            automatalabel.setIcon(Q4);
            Cincuenta();
        }else if(liRB.isSelected()){
            automatalabel.setIcon(Q5);
            Cincuenta();
        }else if(litrosRB.isSelected()){
            automatalabel.setIcon(Q6);
            Cincuenta();
        }else if(seisRB.isSelected() == false || liRB.isSelected() == false || litrosRB.isSelected() == false){
            JOptionPane.showMessageDialog(rootPane, "Porfavor escoja un tipo de jugo");
            automatalabel.setIcon(Q0);
        }
    }//GEN-LAST:event_cincuentabtnActionPerformed

    private void cienbtnActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cienbtnActionPerformed
        if(seisRB.isSelected()){
            automatalabel.setIcon(Q4);
            Cien();
        }else if(liRB.isSelected()){
            automatalabel.setIcon(Q5);
            Cien();
        }else if(litrosRB.isSelected()){
            automatalabel.setIcon(Q6);
            Cien();
        }else if(seisRB.isSelected() == false || liRB.isSelected() == false || litrosRB.isSelected() == false){
            JOptionPane.showMessageDialog(rootPane, "Porfavor escoja un tipo de jugo");
            automatalabel.setIcon(Q0);
        }
    }//GEN-LAST:event_cienbtnActionPerformed

    private void CambioBtn1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_CambioBtn1ActionPerformed
        automatalabel.setIcon(Q7);
        cam = din - 0;
        if (cam < 0 ){
                JOptionPane.showMessageDialog(rootPane, "Lo siento no tienes suficiente dinero");
            }else{
                JOptionPane.showMessageDialog(rootPane, "Recoja su cambio");
                automatalabel.setIcon(Q9);
                LCambio.setText(Double.toString(cam));
                LDinero.setText("");
                cambio(cam);
                din = 0;
                JOptionPane.showMessageDialog(rootPane, "¡Gracias!");
                System.exit(0);
            }
    }//GEN-LAST:event_CambioBtn1ActionPerformed
    
    public static void main(String args[]) {
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                new Maquina().setVisible(true);
            }
        });
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton CambioBtn1;
    private javax.swing.JButton CancelarBtn;
    private javax.swing.JButton ComprarBtn;
    private javax.swing.JLabel LCambio;
    private javax.swing.JLabel LDinero;
    private javax.swing.JLabel automatalabel;
    private javax.swing.JLabel cam1;
    private javax.swing.JLabel cam2;
    private javax.swing.JLabel cam3;
    private javax.swing.JLabel cam4;
    private javax.swing.JLabel cam5;
    private javax.swing.JButton cienbtn;
    private javax.swing.JButton cincobtn;
    private javax.swing.JButton cincuentabtn;
    private javax.swing.JButton diezbtn;
    private javax.swing.JLabel din1;
    private javax.swing.JLabel din2;
    private javax.swing.JLabel din3;
    private javax.swing.JLabel din4;
    private javax.swing.JLabel din5;
    private javax.swing.JInternalFrame jInternalFrame1;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JLabel jugo1;
    private javax.swing.JLabel jugo2;
    private javax.swing.JLabel jugo3;
    private javax.swing.JRadioButton liRB;
    private javax.swing.JRadioButton litrosRB;
    private javax.swing.JRadioButton seisRB;
    private javax.swing.JButton veintebtn;
    // End of variables declaration//GEN-END:variables
}
