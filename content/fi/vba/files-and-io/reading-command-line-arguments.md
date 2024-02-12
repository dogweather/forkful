---
title:                "Komentoriviparametrien lukeminen"
aliases: - /fi/vba/reading-command-line-arguments.md
date:                  2024-02-01T21:59:14.314566-07:00
model:                 gpt-4-0125-preview
simple_title:         "Komentoriviparametrien lukeminen"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/vba/reading-command-line-arguments.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Komennoriviparametrien lukeminen Visual Basic for Applications (VBA):ssa liittyy parametrien käyttöön ohjelmasi suorituksen yhteydessä. Tätä tekniikkaa käytetään usein ohjelman toiminnan tai tulosteen muokkaamiseen ilman käyttäjän vuorovaikutusta, mikä tekee automaatio- ja skriptitehtävistä huomattavasti suoraviivaisempia ja monipuolisempia.

## Kuinka:

Toisin kuin monet suoraviivaisemmat ohjelmointiympäristöt, VBA:lla ei ole sisäänrakennettua ominaisuutta komentoriviparametrien suoraan lukemiseen perinteisessä mielessä, koska se on ensisijaisesti suunniteltu upotettavaksi Microsoft Office -sovelluksiin. Kuitenkin, hieman luovuutta käyttämällä, voimme hyödyntää Windows Script Hostia (WSH) tai kutsua ulkoisia API:eja saavuttaaksemme samankaltaista toiminnallisuutta. Tässä on käytännöllinen kiertotapa käyttäen WSH:tä:

1. **Luo VBScript, joka välittää argumentit VBA:han:**

   Kirjoita ensin VBScript-tiedosto (*yourScript.vbs*), joka käynnistää VBA-sovelluksesi (esim. Excel-makro) ja välittää komentoriviparametrit:

```vb
Set objExcel = CreateObject("Excel.Application")
objExcel.Workbooks.Open "C:\YourMacroWorkbook.xlsm"
objExcel.Run "YourMacroName", WScript.Arguments.Item(0), WScript.Arguments.Item(1)
objExcel.Quit
```

2. **Käytä argumentteja VBA:ssa:**

   VBA-sovelluksessasi (*YourMacroWorkbook.xlsm*), muokkaa tai luo makro (*YourMacroName*), jotta se hyväksyy parametreja:

```vb
Sub YourMacroName(arg1 As String, arg2 As String)
    MsgBox "Argumentti 1: " & arg1 & " Argumentti 2: " & arg2
End Sub
```

3. **Suorita skriptisi:**

   Suorita VBScript komentoriviltä, välittäen tarvittavat argumentit:

```shell
cscript yourScript.vbs "Hei" "Maailma"
```

   Tämän pitäisi johtaa VBA-makrosi suorittamiseen argumenttien "Hei" ja "Maailma" kanssa, näyttäen ne viestiruudussa.

## Syväsukellus:

Historiallisessa kontekstissa, VBA kehitettiin laajentamaan Microsoft Office -sovellusten kykyjä, ei standalone-ohjelmointiympäristönä. Täten suora vuorovaikutus komentorivin kanssa on sen ensisijaisen toiminta-alueen ulkopuolella, mikä selittää sisäänrakennetun tuen puuttumisen komentoriviparametrien lukemisessa.

Yllä esitelty menetelmä, vaikka tehokas, on enemmän kiertotapa kuin natiivi ratkaisu, hyödyntäen ulkoista skriptiä kuilun ylittämiseksi. Tämä lähestymistapa voi tuoda mukanaan monimutkaisuutta ja mahdollisia turvallisuushuolia, koska se vaatii makrojen sallimista ja mahdollisesti turva-asetusten laskemista suorittaakseen.

Tehtäviin, jotka ovat raskaasti riippuvaisia komentoriviparametreista tai tarvitsevat saumattomampaa integraatiota Windows-käyttöjärjestelmän kanssa, muut ohjelmointikielet kuten PowerShell tai Python saattavat tarjota robustimmat ja turvallisemmat ratkaisut. Nämä vaihtoehdot tarjoavat suoran tuen komentoriviparametreille ja soveltuvat paremmin standalone-sovelluksiin tai skripteihin, jotka vaativat ulkoista syötettä käyttäytymisensä dynaamiseen muokkaamiseen.
