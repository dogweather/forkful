---
title:                "JSON के साथ काम करना"
date:                  2024-01-19
html_title:           "Arduino: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/working-with-json.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

JSON (JavaScript Object Notation) एक data format है जो data exchange के लिए use होता है, मुख्यतः वेब applications में। Programmers इसका use करते हैं क्योंकि यह समझने में आसान होता है और language-independent होता है।

## कैसे करें:

```Python
import json

# JSON डेटा को Python ऑब्जेक्ट में बदलना:
jsonData = '{"name": "Amit", "age": 30, "city": "Delhi"}'
pythonObj = json.loads(jsonData)
print(pythonObj["name"])  # Output: Amit

# Python ऑब्जेक्ट को JSON डेटा में बदलना:
pythonDict = {'name': 'Seema', 'age': 25, 'city': 'Mumbai'}
jsonStr = json.dumps(pythonDict, ensure_ascii=False)
print(jsonStr)  # Output: {"name": "Seema", "age": 25, "city": "Mumbai"}
```

## गहराई से जानकारी:

JSON 2000 के दशक में popular हुआ। यह XML के alternative के रूप में उभरा क्योंकि यह ज्यादा lightweight और readable है। JSON, data को key-value pairs में स्टोर करता है। Python में `json` library JSON को parse करने और generate करने के लिए standard tool है। अन्य alternatives में YAML और BSON हैं, लेकिन JSON का use इसकी simplicity और व्यापक support के कारण प्रधान है।

## देखें भी:

- JSON के बारे में और जानकारी के लिए: [JSON वेबसाइट](https://www.json.org/json-en.html)
- Python की `json` library का documentation: [Python JSON library](https://docs.python.org/3/library/json.html)
- JSON online viewer और editor: [JSON Editor Online](https://jsoneditoronline.org/)
