---
title:                "Java: नए प्रोजेक्ट की शुरुआत"
programming_language: "Java"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/starting-a-new-project.md"
---

{{< edit_this_page >}}

## क्यों

हम अक्सर किसी न किसी उत्पादन या संबंधित काम को प्रोग्रामिंग से सुलभ बनाने के लिए एक नया प्रोजेक्ट शुरू करते हैं। यह हमें समस्या को हल करने और नए और उत्कृष्ट तरीकों का आविष्कार करने के लिए अनुमति देता है।

## कैसे

एक नया प्रोजेक्ट शुरू करना बहुत सरल हो सकता है। पहले, आपको अपने परियोजना के उद्देश्य और लक्ष्य को स्पष्ट रूप से निर्धारित करना होगा। इसके बाद, आपको अपनी पसंद के अनुसार एक प्रोग्रामिंग भाषा और समस्या को हल करने के लिए प्रकार को चुनना होगा। आगे बढ़ने से पहले, आपको कुछ आसान उदाहरणों के साथ जावा कोडिंग की जानकारी होनी चाहिए। नीचे उदाहरण और उत्पाद के भीतर कोड ब्लॉक दिया गया है। 

```Java
// दुपहिया बनाने के लिए एक मेथड को वर्ग में जोड़ें
public class Bicycle {
  private int gear;    // अभी तक कम स्थान स्विकार नहीं किया गया
  
  // कनस्ट्रक्टर का प्रयोग करने के लिए जवाब
  public Bicycle(int startGear) {
    gear = startGear;
  }
  
  // गियर मान सेट करने के लिए मेथड
  public void setGear(int newValue) {
    gear = newValue;
  }
  
  // गियर मान प्राप्त करने के लिए मेथड
  public int getGear() {
    return gear;
  }
}

// मॉडल संभालने के लिए मैंने भाइक गियर साथ में रखा और उसे प्राप्त किया
public class BicycleDemo {
  public static void main(String[] args) {
    Bicycle myBike = new Bicycle(5);
    myBike.setGear(3);
    System.out.println("गियर स्थापित हुआ है: " + myBike.getGear());
  }
}
```

## गहराई में जाएं

नए प्रोजेक्ट शुर