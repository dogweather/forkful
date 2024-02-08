---
title:                "TOML के साथ काम करना"
aliases:
- hi/haskell/working-with-toml.md
date:                  2024-01-26T04:23:44.145417-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/working-with-toml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
TOML के साथ काम करना हास्केल के साथ TOML (टॉम की स्पष्ट, न्यूनतम भाषा) डेटा को पार्सिंग और जनरेटिंग करने को शामिल करता है। प्रोग्रामर्स इसे कॉन्फिगरेशन फाइल या डेटा इंटरचेंज को आसानी से प्रबंधित करने के लिए करते हैं, जिसमें मजबूत प्रकार की गारंटी और न्यूनतम सिंटैक्स परेशानी होती है।

## कैसे करें:
सबसे पहले, सुनिश्चित करें कि आपके पास एक TOML पार्सिंग लाइब्रेरी है। Haskell के लिए, `htoml` एक लोकप्रिय विकल्प है। आपको इसे अपनी प्रोजेक्ट की निर्भरताओं में जोड़ना होगा।

```Haskell
-- TOML पार्सिंग लाइब्रेरी को आयात करें
import qualified Text.Toml as Toml

-- अपनी कॉन्फिग डेटा संरचना को परिभाषित करें
data Config = Config {
  title :: String,
  owner :: Owner
} deriving (Show)

data Owner = Owner {
  name :: String,
  dob :: Maybe Day -- वैकल्पिक तारीख
} deriving (Show)

-- एक TOML स्ट्रिंग को पार्स करना
main :: IO ()
main = do
  let tomlData = "[owner]\nname = \"Tom Preston-Werner\"\ndob = 1979-05-27T07:32:00Z"
  case Toml.parseTomlDoc "" tomlData of
    Left err -> putStrLn $ "Error: " ++ show err
    Right toml -> print toml -- या पार्स किए गए TOML को आगे प्रोसेस करें
```

नमूना आउटपुट को किसी भी Haskell डेटा प्रकार की तरह संरचित और एक्सेस किया जा सकता है।

## गहराई में डाइव
ऐतिहासिक रूप से, TOML को GitHub के सह-संस्थापक टॉम प्रेस्टन-वर्नर द्वारा कॉन्फिगरेशन फाइलों के लिए YAML और JSON की जटिलताओं के प्रतिक्रिया स्वरूप बनाया गया था। यह JSON से अधिक पठनीय और लिखने में आसान होने पर जोर देता है, और YAML से अधिक सख्त और सरल होता है।

TOML के विकल्पों में JSON और YAML शामिल हैं, प्रत्येक प्रारूप की अपनी खूबियां हैं। JSON सर्वव्यापी और भाषा-तटस्थ है, जबकि YAML एक अधिक मानव-पठनीय प्रारूप प्रदान करता है। TOML इसकी सादगी और स्थिरता के लिए मूल्यवान है, इसके संबंधियों की कुछ कमियों से बचते हुए।

Haskell में कार्यान्वयन आम तौर पर एक लाइब्रेरी को शामिल करता है जो TOML को एक Haskell डेटा प्रकार में पार्स करता है, अक्सर Haskell की उन्नत प्रकार प्रणाली का लाभ उठाकर सहीपन सुनिश्चित करता है। पार्सिंग को पुनरावर्ती अवरोह या कॉम्बिनेटर पार्सिंग के माध्यम से किया जा सकता है, जो कोड की पठनीयता और बनाए रखने योग्यता के साथ दक्षता को संतुलित करता है।

## देखें भी
- `htoml`: https://hackage.haskell.org/package/htoml
- आधिकारिक TOML GitHub रिपॉजिटरी: https://github.com/toml-lang/toml
- डेटा सीरियलाइजेशन प्रारूपों की तुलना: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
