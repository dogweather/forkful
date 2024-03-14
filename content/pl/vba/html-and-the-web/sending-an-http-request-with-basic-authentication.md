---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:57.647993-07:00
description: "Wysy\u0142anie \u017C\u0105dania HTTP z podstawow\u0105 autoryzacj\u0105\
  \ w Visual Basic for Applications (VBA) dotyczy dost\u0119pu do zasob\xF3w sieciowych\
  \ chronionych przez dane\u2026"
lastmod: '2024-03-13T22:44:35.231414-06:00'
model: gpt-4-0125-preview
summary: "Wysy\u0142anie \u017C\u0105dania HTTP z podstawow\u0105 autoryzacj\u0105\
  \ w Visual Basic for Applications (VBA) dotyczy dost\u0119pu do zasob\xF3w sieciowych\
  \ chronionych przez dane\u2026"
title: "Wysy\u0142anie \u017C\u0105dania HTTP z podstawowym uwierzytelnianiem"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądania HTTP z podstawową autoryzacją w Visual Basic for Applications (VBA) dotyczy dostępu do zasobów sieciowych chronionych przez dane uwierzytelniające użytkownika i hasło. Programiści robią to, aby wchodzić w interakcję z zabezpieczonymi API lub usługami sieciowymi w ramach swoich aplikacji opartych na VBA, takich jak automatyzacja zadań w Excelu czy Accessie z danymi z zabezpieczonych punktów końcowych.

## Jak to zrobić:

W VBA można użyć biblioteki `Microsoft XML, v6.0` (MSXML2) do wysyłania żądań HTTP z podstawową autoryzacją. Obejmuje to ustawienie nagłówka `"Authorization"` żądania w celu dołączenia poświadczeń w formacie zakodowanym w base64. Oto krok po kroku:

1. **Odniesienie do MSXML2**: Najpierw upewnij się, że Twój projekt VBA odwołuje się do biblioteki `Microsoft XML, v6.0`. W edytorze VBA przejdź do Narzędzia > Referencje i zaznacz `Microsoft XML, v6.0`.

2. **Utworzenie i wysłanie żądania HTTP**: Użyj poniższego fragmentu kodu VBA jako przewodnika. Zastąp `"your_username"` i `"your_password"` swoimi faktycznymi danymi uwierzytelniającymi i dostosuj adres URL według potrzeb.

    ```vb
    Dim XMLHttp As Object
    Set XMLHttp = CreateObject("MSXML2.XMLHTTP")
    Dim url As String
    url = "http://example.com/api/resource" ' Zastąp rzeczywistym adresem URL
    Dim base64Credentials As String
    base64Credentials = EncodeBase64("your_username:your_password")
    
    XMLHttp.Open "GET", url, False
    XMLHttp.setRequestHeader "Authorization", "Basic " & base64Credentials
    XMLHttp.send
    
    Debug.Print XMLHttp.responseText ' Wypisuje odpowiedź do Okna Natychmiastowego
    ```

3. **Kodowanie poświadczeń w base64**: VBA nie ma wbudowanej funkcji do kodowania base64, ale można użyć tej niestandardowej funkcji `EncodeBase64`:

    ```vb
    Function EncodeBase64(text As String) As String
        Dim arrData() As Byte
        arrData = StrConv(text, vbFromUnicode)
        
        Dim objXML As MSXML2.DOMDocument60
        Dim objNode As MSXML2.IXMLDOMElement
        
        Set objXML = New MSXML2.DOMDocument60
        Set objNode = objXML.createElement("b64")
        
        objNode.dataType = "bin.base64"
        objNode.nodeTypedValue = arrData
        EncodeBase64 = objNode.Text
    End Function
    ```
    
To wyśle żądanie GET do `http://example.com/api/resource` z określonymi poświadczeniami podstawowej autoryzacji i wydrukuje odpowiedź.

## Pogłębienie

Podejście użyte tutaj, choć skuteczne w prostych przypadkach, opiera się na schemacie Podstawowej Autoryzacji, który przesyła poświadczenia w łatwo dekodowalnym formacie (kodowanie base64 nie jest szyfrowaniem). Ze względu na swoją podatność, szczególnie w kontekstach nie-HTTPS, Podstawowa Autoryzacja nie jest zalecana do przesyłania wrażliwych danych przez internet bez dodatkowych warstw bezpieczeństwa, takich jak SSL/TLS.

Historycznie, Podstawowa Autoryzacja była jedną z pierwszych metod rozwijanych do kontrolowania dostępu do zasobów sieciowych. Obecnie, bezpieczniejsze i bardziej elastyczne standardy uwierzytelniania, takie jak OAuth 2.0, są ogólnie preferowane dla nowych aplikacji. Biorąc pod uwagę ograniczenia VBA oraz zewnętrzne zależności wymagane dla bardziej zaawansowanych metod uwierzytelniania, programiści często stosują VBA w środowiskach wewnętrznych lub mniej krytycznych pod względem bezpieczeństwa, lub używają go jako kamienia węgielnego do szybkiego prototypowania pomysłów.

Podczas używania VBA dla żądań HTTP, pamiętaj, że każda wersja biblioteki MSXML może wspierać różne funkcje i standardy bezpieczeństwa. Zawsze używaj najnowszej wersji kompatybilnej z twoją aplikacją, aby zapewnić lepsze bezpieczeństwo i wydajność. Dodatkowo, rozważ ograniczenia środowiskowe i potencjalnie przestarzałe funkcje przy wyborze VBA dla nowych projektów, szczególnie tych wymagających bezpiecznej komunikacji HTTP. Inne środowiska programistyczne lub języki mogą oferować bardziej solidne, bezpieczne i łatwe w utrzymaniu rozwiązania dla podobnych zadań.
