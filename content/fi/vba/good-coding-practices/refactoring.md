---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:10.846321-07:00
description: "Ohjelmoinnissa refaktorointi tarkoittaa koodin rakenteen muuttamista\
  \ muuttamatta sen toimintaa, jotta esimerkiksi sen luettavuus, yll\xE4pidett\xE4\
  vyys tai\u2026"
lastmod: '2024-03-13T22:44:56.409960-06:00'
model: gpt-4-0125-preview
summary: "Ohjelmoinnissa refaktorointi tarkoittaa koodin rakenteen muuttamista muuttamatta\
  \ sen toimintaa, jotta esimerkiksi sen luettavuus, yll\xE4pidett\xE4vyys tai\u2026"
title: Uudelleenkoodaus
---

{{< edit_this_page >}}

## Mikä & Miksi?

Ohjelmoinnissa refaktorointi tarkoittaa koodin rakenteen muuttamista muuttamatta sen toimintaa, jotta esimerkiksi sen luettavuus, ylläpidettävyys tai suorituskyky paranisivat. Ohjelmoijat refaktoroivat tehdäkseen koodista tehokkaampaa, helpommin ymmärrettävää, helpommin muutettavaa tulevaisuudessa ja vähentääkseen virheiden todennäköisyyttä.

## Kuinka:

Harkitaan yksinkertaista esimerkkiä Visual Basic for Applications (VBA) -koodissa, jossa meillä on alirutiini, joka tulostaa työntekijän tiedot. Aluksi koodi on sekavaa, vaikeaa ylläpitää tai laajentaa.

```vb
Sub PrintEmployeeDetails()
    Dim nimi As String
    Dim ikä As Integer
    Dim osasto As String
    nimi = "John Doe"
    ikä = 30
    osasto = "IT"
    
    MsgBox "Nimi: " & nimi & vbCrLf & "Ikä: " & ikä & vbCrLf & "Osasto: " & osasto
End Sub
```

Refaktorointivaihe 1: Ota metodi käyttöön. Yksi yleisimmistä refaktorointitekniikoista on ottaa tietty koodinpätkä ja siirtää se omaan metodiinsa. Tämä tekee koodista modulaarisemman ja helpommin ymmärrettävän.

```vb
Sub PrintEmployeeDetails()
    Dim nimi As String
    Dim ikä As Integer
    Dim osasto As String
    nimi = "John Doe"
    ikä = 30
    osasto = "IT"
    
    NäytäViesti nimi, ikä, osasto
End Sub

Private Sub NäytäViesti(nimi As String, ikä As Integer, osasto As String)
    MsgBox "Nimi: " & nimi & vbCrLf & "Ikä: " & ikä & vbCrLf & "Osasto: " & osasto
End Sub
```

Refaktorointivaihe 2: Käytä rakennetta. Tässä vaiheessa käytetään tietorakennetta liittyvien tietojen säilyttämiseen, mikä parantaa koodin selkeyttä ja tekee ryhmiteltyjen tietojen siirtämisestä helpompaa.

```vb
Type Työntekijä
    nimi As String
    ikä As Integer
    osasto As String
End Type

Sub PrintEmployeeDetails()
    Dim työnt As Työntekijä
    työnt.nimi = "John Doe"
    työnt.ikä = 30
    työnt.osasto = "IT"
    
    NäytäViesti työnt
End Sub

Private Sub NäytäViesti(työnt As Työntekijä)
    MsgBox "Nimi: " & työnt.nimi & vbCrLf & "Ikä: " & työnt.ikä & vbCrLf & "Osasto: " & työnt.osasto
End Sub
```

Nämä vaiheet muuntavat sekavan koodin modulaariseksi, rakenteelliseksi koodiksi, mikä parantaa merkittävästi sen luettavuutta ja ylläpidettävyyttä.

## Syväluotaus

Refaktoroinnin konsepti on yhtä vanha kuin ohjelmointi itse, mutta Martin Fowlerin kirja "Refactoring: Improving the Design of Existing Code" toi sen valtavirtaan, korostaen sen merkitystä ohjelmistokehitysprosessissa. Visual Basic for Applicationsissa refaktorointi voi olla haastavampaa johtuen nykyaikaisemmissa integroiduissa kehitysympäristöissä (IDEs) löytyvien automaattisen refaktoroinnin tukityökalujen puutteesta.

Tämä ei kuitenkaan vähennä sen tärkeyttä. Myös VBA:ssa perusrefaktorointitekniikoiden manuaalinen soveltaminen voi parantaa merkittävästi koodikantaa, tehden siitä siistimpää ja tehokkaampaa. Vaikka VBA:lla ei olekaan samoja moderneja mukavuuksia, hyvän koodisuunnittelun periaatteet ovat yleismaailmallisia. Muista kielistä tulevat kehittäjät saattavat pitää manuaalista prosessia työläänä, mutta arvostavat epäilemättä ajan investointia koodin laadun parantamiseen alusta alkaen.

Vahvempia kehitysympäristöjä tai erityisen monimutkaisia projekteja varten saattaa olla hyödyllistä tutkia vaihtoehtoja, jotka tarjoavat tehokkaampia refaktorointityökaluja tai muuntaa VBA-projektit .NET-kieleksi, jossa Visual Studio tarjoaa laajaa refaktoroinnin tukea. Siitä huolimatta refaktoroinnin periaatteiden ymmärtäminen ja soveltaminen VBA:ssa on arvokas taito, joka korostaa puhtaan, ylläpidettävän koodin kirjoittamisen tärkeyttä riippumatta ympäristöstä.
