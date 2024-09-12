package al;

/**
 *
 * @author lldan
 */

public class AnalizadorLex{
	private String tokens;
	private String lexema;

	public AnalizadorLex(String tokens, String lexema){
		setLexema(lexema);
		setTokens(tokens);	
	}
        
	public void setTokens(String tokens){
		this.tokens = tokens;
	}
        public void setLexema(String lexema){
		this.lexema = lexema;
	}
	
	public String getTokens(){
		return tokens;
	}
        public String getLexema(){
		return lexema;
	}
	
	public void Imprimir(){
		System.out.println("|" + getTokens() + "| " + getLexema() + "|");
	}
}