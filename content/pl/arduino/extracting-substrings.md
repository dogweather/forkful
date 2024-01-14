---
title:    "Arduino: Wycinanie podciągów"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Dlaczego

Istnieje wiele powodów, dla których możesz chcieć wyodrębnić podciągi w swoim programie Arduino. Jest to przydatna umiejętność w wielu projektach, takich jak przetwarzanie danych lub wyświetlanie informacji w różnych formatach. Przeczytaj dalej, aby dowiedzieć się, jak można to zrobić w prosty sposób.

## Jak to zrobić

Aby wyodrębnić podciągi w Arduino, możesz wykorzystać poniższy kod jako przykład:

```
ArduinoString phrase = "Witaj, świecie!";
ArduinoString subphrase = phrase.substring(7, 13);
Serial.println(subphrase);
```

W takim przykładzie, używamy obiektu `ArduinoString` do przechowywania naszego pierwotnego ciągu. Następnie, za pomocą funkcji `substring()`, wybieramy podciąg z 7. do 13. pozycji (licząc od zera). Wynik zostaje zapisany w nowym obiekcie `subphrase`. W tym przypadku, otrzymamy na wyjściu tylko słowo "świecie". 

Można również wykorzystać funkcję `indexOf()` aby znaleźć pozycję określonego znaku lub podciągu wewnątrz innego ciągu. Przykładowo:

```
ArduinoString phrase = "To jest przykładowy ciąg.";
int position = phrase.indexOf("przykładowy");
Serial.println(position);
```

Tutaj, posiadając zmienną `position`, możemy ustalić, na którym miejscu znajduje się słowo "przykładowy" w naszym pierwotnym ciągu (w tym przypadku będzie to pozycja 7).

## Deep Dive

Funkcje `substring()` i `indexOf()` mogą przyjmować różne argumenty, co pozwala na bardziej elastyczne wyodrębnianie podciągów. Na przykład, w `substring()`, możesz podać tylko jedną liczbę jako argument, co spowoduje wyodrębnienie podciągu od tej pozycji do końca pierwotnego ciągu:

```
ArduinoString phrase = "Wyodrębnij mnie!";
ArduinoString subphrase = phrase.substring(10);
Serial.println(subphrase);
```

W tym przypadku, na wyjściu otrzymamy "mnie!".

Funkcja `indexOf()` może również odnaleźć więcej niż jeden wystąpienie danego znaku lub podciągu. W tym celu, użyj argumentu opcjonalnego `offset`, który określa od którego miejsca w ciągu należy rozpocząć szukanie. Przykładowo:

```
ArduinoString phrase = "To jest przykładowy ciąg.";
ArduinoString subphrase = phrase.substring("jest", 10);
Serial.println(subphrase);
```

W tym przypadku, używając opcjonalnego argumentu `offset`, możemy wyodrębnić podciąg od słowa "jest" do piątego wystąpienia znaku spacji.

## Zobacz również

- Dokumentacja dla funkcji `substring()` w Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/
- Dokumentacja dla funkcji `indexOf()` w Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/indexof/
- Przykładowy projekt wykorzystujący wyodrębnianie podciągów w Arduino: https://create.arduino.cc/projecthub/mrmyj/the-simplest-and-smallest-gesture-controlled-glider-e6b448