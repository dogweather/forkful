---
title:    "Bash: Konwertowanie ciągu znaków na małe litery"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

 Konwersja ciągu znaków na małe litery jest niezbędnym krokiem podczas programowania w Bash. Umożliwia to porównywanie i przetwarzanie tekstu w jednolitym formacie.

## Jak

Aby przekonwertować ciąg znaków na małe litery w Bash, użyj funkcji "tr". Należy podać dwie listy znaków, pierwsza zawiera znaki, które zostaną zamienione, a druga zawiera znaki, na które zostaną zastąpione. Następnie przekaż ciąg znaków do funkcji. Na przykład:

```Bash
echo "Słowo" | tr 'A-Z' 'a-z'
```

Ten kod wyświetli słowo "słowo" w konsoli.

## Deep Dive

Istnieje wiele innych sposobów na konwersję ciągu znaków na małe litery w Bash. Można również użyć funkcji "awk", "sed" lub "bash". Ważne jest, aby wybrać odpowiednią metodę w zależności od potrzeb i kontekstu.

Inną istotną rzeczą do zauważenia jest to, że konwersja na małe litery zależy od ustawień języka i lokalizacji systemu. W wielu językach istnieją znaki, które w ogóle nie mają odpowiednika w alfabecie łacińskim, dlatego nie zostaną one uwzględnione w konwersji.

## Zobacz także
- [Dokumentacja Bash na temat funkcji "tr"](https://www.gnu.org/software/bash/manual/html_node/Bash-Programmable-Completion.html#Bash-Programmable-Completion)
- [Inne przydatne polecenia Bash](https://devhints.io/bash)