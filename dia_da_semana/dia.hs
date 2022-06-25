data DiaDaSemana = Seg | Ter | Qua | Qui | Sex | Sab | Dom

main = do
    let dia = Seg
    let string = humor dia
    putStrLn string


humor :: DiaDaSemana -> String
humor Sab = "Feliz"
humor Dom = "Muito Feliz"
humor Sex = "Sextou"
humor _ = "Vamo la..."