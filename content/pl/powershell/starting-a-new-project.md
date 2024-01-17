---
title:                "Rozpoczynanie nowego projektu"
html_title:           "PowerShell: Rozpoczynanie nowego projektu"
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

Cześć Programiści!

## Co i dlaczego?

Za każdym razem, gdy zaczynamy nowy projekt, jesteśmy podekscytowani nowymi wyzwaniami, możliwościami i możliwością rozwoju naszych umiejętności programistycznych. Tworzenie nowych projektów jest nieodłączną częścią naszej pracy i pozwala nam rozwijać nasze umiejętności i kreatywność. 

## Jak to zrobić?

```PowerShell
New-Item -ItemType Directory -Path C:\NowyProjekt
```

Jeśli chcesz utworzyć nowy projekt w wybranej lokalizacji, możesz użyć komendy `New-Item`. W powyższym przykładzie utworzymy nowy folder o nazwie "NowyProjekt" w lokalizacji C:\.

```PowerShell
Get-ChildItem -Path C:\NowyProjekt
```

Aby potwierdzić, że folder został utworzony, można użyć komendy `Get-ChildItem`. W powyższym przykładzie zobaczymy wynik w formie listy plików i folderów w lokalizacji C:\NowyProjekt.

## Deep Dive

Historia PowerShell sięga 2002 roku, kiedy to Microsoft zaczął prace nad nowym narzędziem do automatyzacji procesów. Aktualnie PowerShell jest jednym z najbardziej popularnych narzędzi dla programistów, dzięki jego możliwościom przetwarzania danych w formacie tekstowym. Alternatywnymi narzędziami do tworzenia nowych projektów w PowerShell są Visual Studio Code i PowerShell ISE. 
Implementacja powyższych przykładów jest równie prosta w obu tych narzędziach.

## Zobacz też

- Dokumentacja Microsoft: https://docs.microsoft.com/pl-pl/powershell/
- Wideo tutorial YouTube: <link>
- Strona GitHub prezentująca przykłady kodów: <link>