---
title:                "Lähettämällä http-pyyntö"
html_title:           "Go: Lähettämällä http-pyyntö"
simple_title:         "Lähettämällä http-pyyntö"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan lähettäisi HTTP-pyynnön? Tässä artikkelissa opit miten Go:n avulla voit lähettää HTTP-pyynnön ja miksi se voi olla hyödyllistä.

## Miten

Go tarjoaa helpon tavan lähettää HTTP-pyynnön käyttämällä `http` -pakettia.

```
func main() {
  // Luo HTTP-pyyntö
  req, err := http.NewRequest("GET", "https://example.com", nil)
  if err != nil {
    panic(err)
  }

  // Luo HTTP-clientti
  client := &http.Client{}

  // Lähetä pyyntö ja saa vastaus
  resp, err := client.Do(req)
  if err != nil {
    panic(err)
  }

  // Tulosta vastauksen statuskoodi ja sisältö
  fmt.Println(resp.Status)
  defer resp.Body.Close()
  body, err := ioutil.ReadAll(resp.Body)
  if err != nil {
    panic(err)
  }
  fmt.Println(string(body))
}
```

```
Output: 
200 OK
<!doctype html>
<html>
<head>
  <title>Example Domain</title>
  ...
</body>
</html>
```

## Syventyvä sukellus

HTTP-pyynnön lähettäminen on hyödyllistä monissa tilanteissa, kuten esimerkiksi saatavilla olevien API-rajapintojen käyttämisessä. Go:n `http` -paketti sisältää myös muita hyödyllisiä toimintoja, kuten mahdollisuuden lisätä otsikoita ja tietoja pyyntöön sekä käsitellä vastauksen eri osia erikseen.

See Also:

- [Go HTTP-paketti dokumentaatio](https://golang.org/pkg/net/http/)
- [Go:n viralliset esimerkit HTTP-pyyntöjen tekemisestä](https://gobyexample.com/http-clients)
- [HTTP-pyyntöjen debuggaus Go-ohjelmassa](https://kodfabrik.com/journal/http-debugging-with-go/)