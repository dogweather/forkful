---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:14.314566-07:00
description: "Kuinka: Toisin kuin monet suoraviivaisemmat ohjelmointiymp\xE4rist\xF6\
  t, VBA:lla ei ole sis\xE4\xE4nrakennettua ominaisuutta komentoriviparametrien suoraan\
  \ lukemiseen\u2026"
lastmod: '2024-03-13T22:44:56.417536-06:00'
model: gpt-4-0125-preview
summary: "Toisin kuin monet suoraviivaisemmat ohjelmointiymp\xE4rist\xF6t, VBA:lla\
  \ ei ole sis\xE4\xE4nrakennettua ominaisuutta komentoriviparametrien suoraan lukemiseen\
  \ perinteisess\xE4 mieless\xE4, koska se on ensisijaisesti suunniteltu upotettavaksi\
  \ Microsoft Office -sovelluksiin."
title: Komentoriviparametrien lukeminen
weight: 23
---

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
