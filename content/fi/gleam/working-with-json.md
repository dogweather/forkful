---
title:                "Gleam: Töitä jsonin kanssa"
simple_title:         "Töitä jsonin kanssa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi

Jos haluat käsitellä dataa web-ohjelmoinnissa, sinun täytyy osata työskennellä JSON:n kanssa. Se on tärkeä osa useimpia moderneja sovelluksia ja on tärkeä taito ymmärtää.

## Kuinka

Voit käyttää Gleam-kieltä JSON:n käsittelyyn yksinkertaisesti käyttämällä JSON-pakettia. Se on sisäänrakennettu moduuli, joten sinun ei tarvitse asentaa mitään lisäosia.

### Esimerkki:

```
Gleam import Gleam.JSON

let data = {% raw %}"""{% endraw %}{"name": "John", "age": 25}{% raw %}"""{% endraw %}

let json = Gleam.JSON.decode(data) 

match json {
  Err(error) -> error
  Ok(data) -> 
    {"Nimi": data["name"], "Ikä": data["age"]}
}
```

Tämä esimerkki osoittaa kuinka voit lukea JSON-dataa ja purkaa sen Gleam-tietorakenteeksi. ```decode```-funktio palauttaa joko virheen tai onnistumisen. Jos data on validi JSON, voit käyttää sitä oman sovelluksesi kanssa.

### Esimerkkitulos:

```
{"Nimi": "John", "Ikä": 25}
```

Tässä näet JSON:n datasta puretut nimi- ja ikä-tiedot. Voit käyttää tätä taitoa luoden vaihtelevia JSON-objekteja ja käsitellä niitä Gleam-kielen avulla.

## Syvämmälle

JSON:n käsittelyllä on monia tärkeitä osia, kuten:

- Syötemuodon validointi
- Tiedon käsittely eri muodoissa (kuten numerot, tekstit jne.)
- Virheiden käsittely

Jatkuvalla harjoittelulla ja tutustumalla Gleam-dokumentaatioon voit oppia monia lisätaitoja ja kehittää kykyjäsi JSON:n käsittelyssä.

## Katso myös

- [Gleam dokumentaatio](https://gleam.run/documentation)
- [Gleam JSON-paketti](https://github.com/gleam-lang/gleam/blob/master/lib/json.gleam)