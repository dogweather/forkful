---
title:    "PHP: एक टेक्स्ट फ़ाइल को पढ़ना"
keywords: ["PHP"]
---

{{< edit_this_page >}}

मात्र १-२ वाक्यों में कहे तो, यह पढ़ना क्यों है *why* आप किसी पाठ फाइल को पढ़ने में लगे हो ?

## क्यों

पाठ फाइल को पढ़ना आपको कई प्रयोगों में आसानी से उपयोगी हो सकता है। इससे आप अधिक डेटा को एक साथ प्रोसेस कर सकते हैं और इसे आपको डेटा में स्ट्रक्चर का पालन करने में भी मदद मिलेगी जिससे आपको डेटा को ध्यानपूर्वक प्रोसेस कर सकते हैं। एक अन्य उदाहरण है आपको विशेष लाइन को पाठ फाइल में रखने के लिए।

## कैसे करे

```PHP
<?php
// फाइल को खोलें
$file = fopen('textfile.txt', 'r') or die("Unable to open file!");

// फाइल को एक बार में पढ़ें
echo fread($file,filesize('textfile.txt'));

// फाइल को बंद करें
fclose($file);
?>
```

बाहर जाने के प्रकार जो प्राप्त करेंगे ।

```
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed eu mi sit amet nibh volutpat maximus. Pellentesque eget velit justo. Sed vehicula nunc ut lorem rhoncus, ac pretium mi euismod. Etiam auctor molestie diam at lobortis. Mauris hendrerit vulputate tortor, ut porttitor magna fringilla viverra. 
```

## गहराई में जाओ

पाठ फाइल को पढ़ने से पहले, फाइल को खोलने के लिए `fopen()` फ़ंक्शन का इस्तेमाल करना जरूरी होगा। इसके बाद, आप `fread()` फ़ंक्शन का उपयोग करके फाइल का उपयोग करके फाइल को पढ़ सकते हैं। अंत में, सही प्रकार से प्रोसेस करने के बाद, फाइल को `fclose()` फ़ंक्शन का इस्तेमाल करके बंद कर सकते हैं।

निश्चित रूप से, इस तरह से कोडिंग आपको बहुत मदद देगा अपनी आवश्यकता के मुताबिक लाइन का प्रबंधन करने में और अधिक समय बचाने के लिए।

##