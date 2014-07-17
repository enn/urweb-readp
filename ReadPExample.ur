
fun main () =
  t <- source "";
  s <- source "";
  return <xml>
    <body>
      <h1>ReadP Example</h1>
      <ctextbox source={t}/>
      <button value="test" onclick={fn _ => v <- get t; set s v}/>
	<br/>
	<dyn signal={v <- signal s; return <xml>{[v]}</xml>}/>
    </body>
  </xml>

