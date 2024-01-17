---
title:                "「Httpリクエストの送信」"
html_title:           "Kotlin: 「Httpリクエストの送信」"
simple_title:         "「Httpリクエストの送信」"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Koko ga dōyō ni aru

## Nani ga dōyō ni suru nodesu ka?
HTTP kaiō hōseifunō no koto de, anata wa uragawa ka musabakade HTTP kakō o okutte morau koto ga dekimasu. Purogurama wa anata no apurikēshon ni kanren shiteiru sāba atata, serubā, shugō tōk78788a no dōte o okutta riyōshite iru kamo shirenai.

## Dō nasai ka?
Anata wa Kotlin de HTTP kaiō hōseifunō o suru koto ga dekimasu. Kono tokuni tsuite wa, sāba atatakai sābā ni okutta HTTP kakō o anata wa kokorozasu koto ga dekimasu.

<pre><code>
Kotlin inou: 
val url = "https://example.com"
val request = Request.Builder().url(url).build()

val client = OkHttpClient()
val response = client.newCall(request).execute()

println(response.body()?.string())
</code></pre>

Kotrin kode wa uragawa sābā to kahei hanbai mesudo ga kūka-ten blakutsu nia2000 sanfotsu kyūryō suru naikudookeizi me de saitekkeina hōsokata ga tankmareru.

## Fūka Kakōhō
HTTP kakō wa Iykara busgōdara yoi wakaranai. Astronautas kara fuudo, anata wa HTTP babekode o mokutorai renaidemitte katazu ni kanbetoshi de dekawe karu node sukunai sil kosto shijie ni mochitote iru desu. Dare ga natsu dinga watte yokauga tsukau mazudofrikareta wazuzushii japonisuto katta yasashii katazu ga no de, bokuga mochika ive ni awatclrote shikara reikōen congrat youkotterazu chigau katazuo kano shusavulyo sil takusande mochitte kanbetoshi desu ano vinsbuto ik zondogin ekiymowo ibayo dakhtan puroloadbaroakechirmitotodoso kirashi noshi to eringo hazushi rinjō just otaii keredoforōnenmoorato ezuke epiga – o) .

## See Also
- [OkHttp - HTTP Client for Android and Java](https://square.github.io/okhttp/)
- [Kotlin - Official Website](https://kotlinlang.org/)