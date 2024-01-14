---
title:                "Swift: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

W dzisiejszych czasach tekstowe pliki są nieodłączną częścią programowania i stanowią ważny element tworzenia oprogramowania. Dlaczego więc warto poznać ich tworzenie i jak to zrobić? Jakie korzyści płyną z umiejętności pisania plików tekstowych? Przeczytaj ten artykuł, aby dowiedzieć się więcej!

## Dlaczego
Pisanie plików tekstowych jest nie tylko ważnym aspektem programowania, ale także daje wiele przydatnych możliwości. Pozwala na zapisywanie danych w łatwy do odczytania i zrozumienia formacie, umożliwiając przechowywanie informacji bezpośrednio w plikach na komputerze. Dzięki temu można szybko przetwarzać i analizować duże ilości danych, co jest szczególnie ważne przy tworzeniu zaawansowanych aplikacji. Teraz zapoznamy się z krótkim przykładem kodu, który pokaże, jak w praktyce tworzyć i zapisać plik tekstowy w języku Swift.

## Jak To Zrobić

```Swift
// Tworzenie nowego pliku tekstowego o nazwie "sampleFile.txt" w bieżącym katalogu
let fileURL = URL(fileURLWithPath: "sampleFile.txt")
// Zapisywanie zawartości w postaci String
let fileContents = "To jest przykład tekstu w naszym pliku"
// Zapisywanie pliku na dysku
try fileContents.write(to: fileURL, atomically: true, encoding: .utf8)
```
W powyższym przykładzie korzystamy z klasy `URL` w celu utworzenia ścieżki do pliku, a następnie zapisujemy zawartość w postaci tekstu w zmiennej `fileContents`. Na koniec wykorzystujemy funkcję `write` w celu zapisania pliku na dysku. Zauważ, że musimy również podać kodowanie tekstu - w tym przypadku używamy standardowego kodowania UTF-8.

## Deep Dive
Tworzenie plików tekstowych to jedna z podstawowych czynności, którą powinien umieć każdy programista. Jednak warto poznać również bardziej zaawansowane funkcje związane z tym tematem. Na przykład, można dodać nowe linie do pliku tekstowego, wykorzystując funkcję `append` zamiast `write`. Istnieje też możliwość odczytu już istniejącego pliku - w tym celu wykorzystuje się klasę `Data` lub `String`, aby wczytać zawartość do zmiennej i przetwarzać ją dalej w kodzie.

## See Also
Jeśli chcesz dowiedzieć się więcej o tworzeniu plików tekstowych w języku Swift, polecamy zapoznanie się z poniższymi źródłami:
- [Dokumentacja Apple na temat tworzenia plików w Swift](https://developer.apple.com/documentation/foundation/filemanager/)
- [Artykuł na blogu Ray Wenderlich na temat obsługi plików tekstowych w Swift](https://www.raywenderlich.com/365-property-lists-tutorial-for-ios-how-to-save-data-in-apps)
- [Wideo na YouTube na temat tworzenia i odczytu plików tekstowych w Swift](https://www.youtube.com/watch?v=s1F_mX5d2gg)

Dzięki wykorzystaniu powyższych informacji, już teraz możesz bez problemu tworzyć i obsługiwać pliki tekstowe w swoich aplikacjach w języku Swift. Będziesz mógł efektywnie zarządzać danymi, co w dłuższej perspektywie ułatwi Ci pracę i pozwoli osiągnąć lepsze wyniki.