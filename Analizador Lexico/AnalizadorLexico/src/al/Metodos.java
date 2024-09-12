package al;

import java.io.*;

/**
 *
 * @author lldan
 */

public class Metodos {
    private Character del[] =  {'(',')',';','[',']','{','}'};
    private String reservado[] = {"main","static","void","int"," true"," false"," null","if","else","for","while","float","class","new"," super","null",
                                    "switch"," do","private"," public","return","byte","char"," final","long","finally","try","break","import","throw","this",
                                    "default","package"," to","open","export","provides","modules","opens","with","requires","transitive"," uses","short",
                                    "extends","boolean","continue","double","String"};
    private Character op[] = {'+','-','*','/','%','!','<','>','=','&','|','?',':','^',',','.'};
    private Character au = 'x';
    private String textoCom = "";
        
    public void Cartel(){	
	System.out.println("_________________________________________________________");
	System.out.print("|                    Analizador Lexico                   |\n");
	System.out.print("|       Al terminar presiona la tecla Space+Enter        |\n");
	System.out.println("|________________________________________________________|");
	System.out.println();
    }

    public Character[] Tabla(){
        String res = "";
        int longitud = 0;
        Character txt[];
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        try{
            System.out.print("Introduce el texto: ");
            do{
                res += in.readLine();
                textoCom = res;
            }while(!res.endsWith(" "));
                System.out.println("\n");
                System.out.print("|   Tokens    |   Lexema    |\n");
            }catch(IOException ioe){
                System.out.println("Excepcion: " + ioe);
            }
        longitud = res.length();
        txt = new Character[longitud];
        for(int i = 0; i < longitud ; i++){
            txt[i] = res.charAt(i);
        }
        return txt;
    }
        
    public void Digito(Character txt[]){
        String digito = "";
        for(int i=0; i<txt.length; i++){
            if(au.isDigit(txt[i])){
                digito = "     " + txt[i].toString() + "      ";
                AnalizadorLex out = new AnalizadorLex("   Digito    ", digito);
                out.Imprimir();
            }
        }
    }

    public void Delimitador(Character txt[]){
        String delimitador = "";
        for(int i=0; i<txt.length; i++){
            for(int j=0; j<del.length; j++){
                if(txt[i].equals(del[j])){
                    delimitador = "     " + txt[i].toString() + "      ";
                    AnalizadorLex out = new AnalizadorLex(" Delimitador ", delimitador);
                    out.Imprimir();
                }			
            }
        }
    }

    public void Identificador(Character txt[]){
        String identificador = "";
        for(int i=0; i<txt.length; i++){
            if(au.isJavaIdentifierStart(txt[i])){
                identificador = "     " + txt[i].toString() + "      ";
                AnalizadorLex out = new AnalizadorLex("Identificador", identificador);
                out.Imprimir();	
            }
        }
    }
    
    public void Operador(Character txt[]){
        String operador = "";
        for(int i=0; i<txt.length; i++){
            for(int j=0; j<op.length; j++){
                if(txt[i].equals(op[j])){
                    operador = "     " + txt[i].toString() + "      ";
                    AnalizadorLex out = new AnalizadorLex("  Operador   ", operador);
                    out.Imprimir();
                }			
            }
        }
    }
    
    public void Reservado(Character txt[], String texto){
        String reserv = "";
            for(int j=0; j<reservado.length; j++){                   
                int buscar = textoCom.indexOf(reservado[j]);
                if(buscar != -1){
                    reserv = "     " + reservado[j].toString() + "      ";
                    AnalizadorLex out = new AnalizadorLex("  Reservado  ", reserv);
                    out.Imprimir();
                }	
        }
    }
}