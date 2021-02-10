-- 1. ehPar: escreva uma função que receba um número e retorne True caso este número seja par, e False caso contrário.

ehPar x = if rem x 2 == 0 then True else False

-- 2. anoIdade: escreva uma função que receba o ano de nascimento e retorne a idade.

anoIdade x = 2020 - x

-- 3. permiteDirigir: utilizando a funçao anoIdade, escreva uma função que dado o ano de nascimento, retorne True caso tenha idade igual ou superior a 18 anos, e False caso contrário.

permitirDirigir x = if anoIdade x > 17 then True else False 

-- 4. zeroPosNeg: escreva uma função que receba um número inteiro e retorne: "zero" caso seja 0, "positivo" caso seja maior que 0 e "negativo" caso seja menor que 0.

zeroPosNeg x
            |x == 0 = "zero"
            |x > 0 = "positivo"
            |x < 0 = "negativo"

-- 5. media3: escreva uma função que receba 3 números, calcule e retorne a parte inteira da média.

media3 x y z = div (x + y + z) 3 

-- 6. areaSala: escreva uma função que receba as medidas de comprimento e largura de uma sala, e retorne a sua área.

areaSala x y = x * y

-- 7. minHoras: escreva uma função que receba um período de tempo em minutos, e retorne quantas horas equivalem.

minHoras x = div x 60

-- 8. dobroOuTriplo: escreva uma função que calcule o dobro de um número caso ele seja positivo, e seu triplo caso seja negativo.

dobroOuTriplo x
               | x > 0 = 2*x
               | x < 0 = 3*x

-- 9. ehAprovado: escreva uma função que receba 3 notas, calcule a média (utilize a função media3 criada anteriormente), e retorne "aprovado" caso a média seja maior ou igual a 7, e "reprovado" caso contrário.

ehAprovado x y z = if (media3 x y z) >= 7 then "Aprovado" else "Reprovado"

-- 10. somaDigitos: escreva uma função que receba um número natural de 0 a 999 e retorna a soma de seus dígitos.

somadigitos x 
