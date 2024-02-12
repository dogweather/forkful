---
title:                "Pobieranie strony internetowej"
date:                  2024-02-01T21:53:44.072532-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pobieranie strony internetowej"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/vba/downloading-a-web-page.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Pobieranie strony internetowej w Visual Basic for Applications (VBA) polega na pobraniu zawartości HTML strony internetowej z Internetu. Programiści często wykonują to zadanie, aby programowo przetwarzać lub analizować zawartość stron internetowych z poziomu Excela, Accessa lub innych aplikacji Office.

## Jak to zrobić:

Aby pobrać stronę internetową w VBA, można skorzystać z biblioteki Microsoft XML, wersja 6.0 (MSXML6), która umożliwia wykonywanie żądań HTTP serwera. Zanim przejdziesz do kodu, upewnij się, że włączyłeś to odniesienie w swoim edytorze VBA, przechodząc do `Narzędzia` -> `Odniesienia` i zaznaczając `Microsoft XML, wersja 6.0`.

Oto prosty przykład, jak pobrać zawartość HTML strony internetowej:

```basic
Sub PobierzStroneWWW()
    Dim request As Object
    Dim url As String
    Dim response As String

    ' Inicjalizacja obiektu żądania XML HTTP
    Set request = CreateObject("MSXML2.XMLHTTP")

    url = "http://www.example.com"

    ' Otwarcie synchronicznego żądania
    request.Open "GET", url, False

    ' Wysłanie żądania do serwera
    request.send

    ' Pobranie tekstu odpowiedzi
    response = request.responseText

    ' Wyświetlenie odpowiedzi w oknie natychmiastowym (do celów debugowania)
    Debug.Print response

    ' Sprzątanie
    Set request = Nothing
End Sub
```

Uruchomienie tej subrutyny spowoduje wydrukowanie HTML-a `http://www.example.com` w oknie Natychmiastowym w edytorze VBA. Należy zauważyć, że parametr `False` w metodzie `Open` sprawia, że żądanie jest synchroniczne, co oznacza, że kod będzie czekał, aż strona internetowa zostanie pobrana, zanim przejdzie do następnej linii.

## Głębsze zanurzenie

Technika pokazana polega na użyciu MSXML, implementacji przez Microsoft standardu XML HTTP Request, często używanego do żądań AJAX w rozwoju stron internetowych. Ten komponent jest częścią stosu technologicznego Microsoftu od dawna, co czyni go solidnym wyborem dla żądań sieciowych w VBA.

Jednak opieranie się na MSXML i VBA do pobierania i analizowania zawartości stron internetowych może być ograniczające, szczególnie z nowoczesnymi aplikacjami internetowymi, które intensywnie wykorzystują JavaScript do dynamicznego renderowania treści. Te ograniczenia mogą sprawić, że inne języki lub narzędzia, takie jak Python z bibliotekami takimi jak BeautifulSoup lub Selenium, będą bardziej odpowiednie do zadań związanych z web scrapingiem, ze względu na ich zdolność do wykonania JavaScriptu i obsługi skomplikowanych interakcji z witrynami.

Mimo to, dla prostych zadań, które wymagają pobrania prostych treści HTML lub przy pracach w ramach aplikacji Office, VBA pozostaje praktycznym narzędziem. Jego integracja z pakietem Office umożliwia bezpośrednią manipulację dokumentami na podstawie zawartości sieciowej, oferując unikatową zaletę dla konkretnych przypadków użycia.
