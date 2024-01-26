---
title:                "XML के साथ काम करना"
date:                  2024-01-26T04:31:36.223437-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML के साथ काम करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/working-with-xml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
Elm के साथ XML का काम करने का मतलब है XML दस्तावेज़ों को पार्स करना, बदलना, और उत्पन्न करना। यह XML को उनके डेटा प्रारूप के रूप में उपयोग करने वाली कई वेब सेवाओं और विरासत प्रणालियों के साथ बातचीत करने के लिए किया जाता है।

## कैसे:
Elm में, आप `elm/xml` पैकेज का उपयोग करके XML से निपटते हैं। यहाँ एक XML स्निपेट को पार्स करने का एक संक्षिप्त दृश्य है:

```Elm
import Xml.Decode exposing (..)
import Xml.Decode.Pipeline exposing (..)

xmlString = """
<book id="123">
    <title>Elm in Action</title>
    <author>Robin Heggelund Hansen</author>
</book>
"""

type alias Book =
    { id : String
    , title : String
    , author : String
    }

bookDecoder : Decoder Book
bookDecoder =
    decode Book
        |> required "id" (attribute "id")
        |> required "title" (child "title" (content text))
        |> required "author" (child "author" (content text))

case Xml.Decode.fromString bookDecoder xmlString of
    Ok book ->
        -- यहां डिकोड की गई पुस्तक के साथ कुछ करें
        Debug.toString book

    Err error ->
        -- त्रुटियों को संभालें
        Debug.toString error
```

मानते हैं कि कोई त्रुटि नहीं है, उदाहरण आउटपुट:

```Elm
"{ id = \"123\", title = \"Elm in Action\", author = \"Robin Heggelund Hansen\" }"
```

## गहन जानकारी
XML (एक्सटेंसिबल मार्कअप लैंग्वेज) 90 के दशक के अंत से है, जब वेब पाठ-भारी था और डेटा को वहन करने के लिए एक संरचित, फिर भी लचीले तरीके की आवश्यकता थी। शब्दावली और जटिलता के कारण, XML ने JSON के मुकाबले कुछ मैदान गंवाया है। हालांकि, XML अभी भी प्रचलित है, विशेष रूप से एंटरप्राइज़ परिवेश या SOAP जैसे प्रोटोकॉल में।

Elm का XML के प्रति दृष्टिकोण कार्यात्मक और प्रकार-सुरक्षित है। `elm/xml` पैकेज का उपयोग करना Elm के विशिष्टता और विश्वसनीयता के दर्शन को अपनाना है। पार्सिंग के मामले में, पैकेज डिकोडर्स की एक श्रेणी प्रदान करता है जिन्हें आप XML संरचना को संभालने के लिए संयोजित करते हैं।

JavaScript के DOMParser या Python के ElementTree जैसे विकल्पों की तुलना में, Elm की विधि अधिक शब्दावली प्रतीत हो सकती है लेकिन सुरक्षा सुनिश्चित करती है। कोई रनटाइम अपवाद नहीं होते हैं जैसे कि गुम फ़ील्ड या प्रकार असंगतियों के लिए; अगर कुछ गलत है, तो आपको एक संकलन-समय त्रुटि मिलती है।

`elm/xml` डिकोड फ़ंक्शन्स Elm प्रकारों को XML नोड्स में मैप करने पर आधारित हैं। आप अपने डेटा के आकार को दर्शाने वाले डिकोडर्स का निर्माण करते हैं, सुनिश्चित करते हैं कि आपका Elm ऐप XML को उतनी ही सख्ती से संभालता है जितना कि यह अपने स्वयं के आंतरिक डेटा संरचनाओं को करता है।

Elm में XML का उत्पादन कम आम है लेकिन `elm/xml` के समकक्ष `Xml.Encode` के साथ हासिल किया जा सकता है।

## भी देखें
- Elm गाइड जो JSON के बारे में है जो XML माइंडसेट के लिए भी लागू है: [https://guide.elm-lang.org/interop/json.html](https://guide.elm-lang.org/interop/json.html)
- XML स्वयं की गहन समझ के लिए W3C द्वारा XML मानक: [https://www.w3.org/XML/](https://www.w3.org/XML/)