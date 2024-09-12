package al;

/**
 *
 * @author lldan
 */

public class AnalizadorLexico {
    public static void main(String carls[]){	
	Character texto[];
        String text = "";
	Metodos obj = new Metodos();
	obj.Cartel();
	texto = obj.Tabla();
	obj.Identificador(texto);
	obj.Delimitador(texto);
	obj.Digito(texto);
        obj.Operador(texto);
        obj.Reservado(texto, text);
    }
}
