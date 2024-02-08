---
title:                "TOML:n kanssa työskentely"
aliases:
- fi/vba/working-with-toml.md
date:                  2024-02-01T22:06:44.247893-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML:n kanssa työskentely"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/vba/working-with-toml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

TOML, joka tarkoittaa Tom's Obvious, Minimal Language (Tomin ilmeinen, minimalistinen kieli), on datan sarjoitustapa, jota käytetään pääasiassa konfiguraatiotiedostoissa. Ohjelmoijat hyödyntävät TOMLia sen luettavuuden ja helpon datastruktuureihin kartoittamisen ansiosta, mikä mahdollistaa suoraviivaisen sovellusten konfiguroinnin erilaisissa ohjelmointiympäristöissä, mukaan lukien Visual Basic for Applications (VBA).

## Kuinka:

TOMLin kanssa työskentely VBA:ssa sisältää TOML-tiedoston jäsentämisen, jotta konfiguraatiot tai asetukset voidaan lukea VBA-projektiisi. VBA:lla ei ole sisäänrakennettua tukea TOMLille, joten yleensä käytetään jotakin jäsentäjää tai muunnetaan TOML-tiedot muotoon, jonka kanssa VBA pystyy helposti työskentelemään, kuten JSON tai XML. Tässä on, kuinka voit manuaalisesti jäsentää yksinkertaisen TOML-konfiguraatiotiedoston:

1. **Esimerkki TOML-tiedostosta** (`config.toml`):
```
title = "TOML Esimerkki"

[database]
server = "192.168.1.1"
ports = [ 8000, 8001, 8002 ]
connection_max = 5000
enabled = true
```

2. **VBA-koodi TOMLin jäsentämiseen**:

Olettaen, että TOML-sisältö on luettu merkkijonovariable `tomlStr`, seuraava VBA-koodi osoittaa yksinkertaisen lähestymistavan `[database]`-osion jäsentämiseen:

```vb
Function ParseTOML(tomlStr As String)
    Dim lines() As String
    lines = Split(tomlStr, vbCrLf)
    
    Dim config As Object
    Set config = CreateObject("Scripting.Dictionary")
    Dim currentSection As String
    currentSection = ""
    
    Dim i As Integer
    For i = 0 To UBound(lines)
        Dim line As String
        line = Trim(lines(i))
        If InStr(line, "[") > 0 And InStr(line, "]") > 0 Then
            currentSection = Mid(line, 2, Len(line) - 2)
            Set config(currentSection) = CreateObject("Scripting.Dictionary")
        ElseIf InStr(line, "=") > 0 Then
            Dim parts() As String
            parts = Split(line, "=")
            Dim key As String
            key = Trim(parts(0))
            Dim value As String
            value = Trim(parts(1))
            config(currentSection)(key) = value
        End If
    Next i
    
    'Esimerkki jäsentiedon käyttämisestä
    Debug.Print "Tietokannan palvelin: "; config("database")("server")
End Function
```

3. **Esimerkkituloste** (Välitön Ikkuna):
```
Tietokannan palvelin: 192.168.1.1
```

## Syventävä tarkastelu

TOMLin käytännön hyväksyntä kehittäjäyhteisössä osoittaa suuntausta yksinkertaisempiin, inhimillisemmin luettaviin konfiguraatiotiedostoihin, verrattuna aikaisemmin vallalla olleeseen XML:iin. TOMLin suunnitteluajattelu korostaa selkeitä semantiikkoja ja pyrkii suoraviivaiseen jäsentämiseen minimaalisella ylimääräisellä kuormituksella. VBA:ssa TOMLin suora käsittely edellyttää manuaalista jäsentämistä tai ulkoisten työkalujen hyödyntämistä TOMLin muuntamiseksi VBA:lle ystävällisempään muotoon, koska natiivituki puuttuu. Vaikka tämä manuaalinen jäsentämismenetelmä esittelee peruslähtökohdan, ulkoisten kirjastojen tai välimuotoisten formaattien, kuten JSON, käyttö saattaa tarjota vankempia ja virheenkestävämpiä jäsentämisstrategioita. Ottaen huomioon VBA:n laajan integraation Microsoft Officen kanssa, TOMLin muuntaminen JSONiksi ja VBA:n natiivien JSON-jäsentelykykyjen (tarvittaessa) tai kolmannen osapuolen JSON-jäsentimien käyttö voi tarjota virtaviivaisemman työnkulun. Lisäksi, jatkuvasti kehittyvien datan serialisointimuotojen myötä, ohjelmoijien tulisi myös harkita YAML:ää, joka kuten TOML, korostaa ihmisen luettavuutta, mutta tarjoaa erilaisia kompromisseja monimutkaisuuden ja joustavuuden suhteen.
