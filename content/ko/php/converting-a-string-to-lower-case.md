---
title:    "PHP: 문자열을 소문자로 변환하기"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## 왜
문자열을 소문자로 변환하는 일에 참여하는 이유를 알고 싶다면 이 포스트를 읽어보세요.

## 하우 투
```PHP
$string = "HELLO WORLD";
echo strtolower($string);

//Output: hello world
```

```PHP
$string = "HeLlO wOrLd";
echo strtolower($string);

//Output: hello world
```

## 딥 다이브
문자열을 소문자로 변환하는 것은 대소문자를 무시하고 비교해야 할 때 유용합니다. 예를 들어 사용자의 이름을 검색하는 경우, 대소문자가 다른 경우도 해당 사용자를 찾을 수 있습니다. 또는 데이터베이스에서 검색하는 경우에도 대소문자를 구분하지 않고 검색하면 더 정확한 결과를 얻을 수 있습니다.

또한 문자열을 소문자로 변환하는 방법은 다양한 언어와 프레임워크에서 유사하게 사용됩니다. PHP에서는 ```strtolower()``` 함수를 사용하지만, 자바스크립트에서는 ```toLowerCase()```를 사용하고, 파이썬에서는 ```lower()```를 사용합니다.

## 세 어울리는 이쪽 글도 보세요
[PHP strtolower() 함수 문서](https://www.php.net/manual/en/function.strtolower.php)  
[JavaScript toLowerCase() 메소드 문서](https://www.w3schools.com/jsref/jsref_tolowercase.asp)  
[Python lower() 메소드 문서](https://www.w3schools.com/python/ref_string_lower.asp)