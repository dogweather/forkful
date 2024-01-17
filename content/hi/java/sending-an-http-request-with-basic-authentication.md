---
title:                "बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना"
html_title:           "Java: बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एचटीटीपी अनुरोध को बुनियादी प्रमाणीकरण के साथ भेजना क्या है, और क्यों प्रोग्रामर इसे करते हैं, यह दो से तीन सेन्टेंस हैं।

## कैसे करें:

यहां, हम आपको एक जावा मेथड का उदाहरण और उसका आउटपुट दे रहे हैं:

```Java
public void sendRequest(){
    String username = "myUsername";
    String password = "myPassword";
    
    String url = "https://www.example.com/api";
    
    //Setting up basic authentication
    String authString = username + ":" + password;
    byte[] authEncBytes = Base64.getEncoder().encode(authString.getBytes());
    String authStringEncoded = new String(authEncBytes);
    
    //Creating HttpUrlConnection
    URL obj = new URL(url);
    HttpURLConnection con = (HttpURLConnection) obj.openConnection();
    
    //Setting request method
    con.setRequestMethod("GET");
    
    //Adding Authorization header
    con.setRequestProperty("Authorization", "Basic " + authStringEncoded);
    
    //Sending request
    int responseCode = con.getResponseCode();
    System.out.println("Response Code: " + responseCode);
    
    //Reading response
    BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
    String inputLine;
    StringBuffer response = new StringBuffer();
    
    while ((inputLine = in.readLine()) != null) {
        response.append(inputLine);
    }
    in.close();
    
    //Printing response
    System.out.println(response.toString());
}
```

उपरोक्त कोड आउटपुट के साथ एक धारणात्मक रूप में निम्न परिणाम देगा:

```
Response Code: 200
{"message": "Success"}
```

## गहराई में जाने:

बुनियादी प्रमाणीकरण के साथ एचटीटीपी अनुरोध को भेजने के लिए कुछ अन्य विकल्प भी हैं। सरल अनुमत आधार प्रणाली (Simple Authorization System) और क्रिप्टोग्राफिक अल्गोरिथ्म भी सबसे आम हैं। इन सभी अल्गोरिथ्म में सर्वश्रेष्ठता को मिलाने के लिए आपको क्लास का सेट करना पड़ता है (चार्कों और शब्दांशों के स्पष्ट नमूने, जैसे, साउंड्स लॉगिंग)।

बुनियादी प्रमाणीकरण के साथ एचटीटीपी अनुरोध भेजने के लिए जावा में क्रिप्टोग्राफिक अल्गोरिथ्म का एक अनुमानित उपयोग दो बोलों में इस DARPA द्वारा किया गया था कि कारण एक अनौपचारिक शीर्षक है। भाषाएँ। अपनी हार्डवेयर और सॉफ्टवेयर को अपग्रेड करते गए । (एंप्ट्ले 2000, 2007 इत्यादि)

## के साथ देखो:

- यह वीडियो जावा अनुरोधों को बेसिक प्रमाणित करना सिखाता है: https://youtu.be/oUa3wqoMLtY
- अपने एपीआई से एचटीटीपी प्रमाणीकरण का उपयोग करने के लिए आलेख: https://www.baeldung.com/java-http-request