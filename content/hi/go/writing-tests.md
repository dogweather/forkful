---
title:    "Go: टेस्ट लिखना"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/go/writing-tests.md"
---

{{< edit_this_page >}}

## क्यों

टेस्टिंग एक विशेषज्ञता है जो हर एक गो प्रोग्रामर का होना चाहिए। यह आपको अपने कोड को बेहतर बनाने में मदद करता है, सुनिश्चित करता है कि सर्वर और प्रॉग्राम लाइब्रेरी काम कर रहे हैं, और अन्य बग्स को पता करने में मदद करता है। हम छोटे-मोटे फीचर या कोड बदलावों पर ज्यादा ध्यान देते हैं, लेकिन बहुत बार हम यह भूल जाते हैं कि अपना एप्लिकेशन की संपूर्णता के लिए टेस्टिंग की आवश्यकता होती है।

## कैसे करें

### टेस्ट केस बनाना 

```Go
func TestAdd(t *testing.T) {
	result := add(2, 3)
	if result != 5 {
		t.Errorf("Expected 5, got %d", result)
	}
}
```

### मॉकिंग का उपयोग करना

```Go
func TestFetchData(t *testing.T) {
	mockHTTPClient := MockHTTPClient{}
	result := fetchData(mockHTTPClient)
	expectedResult := "sample data"
	if result != expectedResult {
		t.Errorf("Expected %v, got %v", expectedResult, result)
	}
}
```

### टेस्ट क्लिनिंग करना 

```Go
func TestDeleteRecord(t *testing.T) {
	recordID := 1
	deleteRecord(recordID)
	// perform necessary checks
}
```

## गहराई में खोजें

टेस्टिंग में और भी कई मुख्य विषय होते हैं, जैसे आधर टेस्टिंग, इन्टीग्रेशन टेस्टिंग, स्टब्स का उपयोग, टेस्ट स्यूइट में मॉकिंग करना और भी बहुत कुछ। यदि आप गो में टेस्टिंग के बारे में और गहराई से जानना चाहते हैं, तो आप इन लिंक्स पर जाएं: 

## देखें भी 

- [Go कोड टेस्ट करना](https://gobyexample.com/testing)
- [गो कोड टेस्टिंग फ्रेमवर्क, गोकोन एक्स्पेक्ट](https://github.com/go-playground/assert)
- [गो कोड टेस्टिंग सुनिश्चित करना](https://blog.alexellis.io/golang-writing-unit-tests/)
- [गो प्रो