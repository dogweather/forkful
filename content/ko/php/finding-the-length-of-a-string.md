---
title:    "PHP: 문자열의 길이 찾기"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 왜 
문자열의 길이를 찾는 일에 참여하는 이유는 프로그래밍에서 매우 중요한 작업이기 때문입니다. 문자열의 길이를 알면 다양한 작업을 할 수 있고, 데이터의 유효성을 검사하는데 도움이 됩니다. 또한 문자열의 길이는 프로그래밍 언어에서 매우 기본적인 작업이므로 반드시 알아야 합니다.

# 하는 방법
```PHP
$string = "안녕하세요?";

// 내장 함수인 strlen()을 사용해 문자열의 길이를 찾는 방법
$length = strlen($string);

echo "문자열의 길이는 $length 입니다.";
```
출력 결과: "문자열의 길이는 7입니다."

## 깊게 파고들기
문자열의 길이를 찾는 방법에 대해 더 자세히 알아보겠습니다. PHP에서는 내장 함수인 `strlen()`을 사용하여 문자열의 길이를 확인할 수 있습니다. 이 함수는 대부분의 다른 프로그래밍 언어에서도 사용하는 일반적인 함수이므로, 여러분이 다른 언어를 배우더라도 동일한 방법으로 문자열의 길이를 찾을 수 있습니다. 그리고 `strlen()` 함수는 UTF-8과 같은 다양한 문자 인코딩에도 대처할 수 있도록 설계되어 있습니다.

# 참고 자료
[PHP 공식 문서 - strlen() 함수](https://www.php.net/manual/en/function.strlen.php)

[ASCII 및 UTF-8 인코딩에 대한 더 자세한 설명](https://www.w3schools.com/charsets/ref_utf_basic_latin.asp)

[다른 프로그래밍 언어에서 문자열의 길이를 찾는 방법들](https://www.guru99.com/string-length-function-php.html)

# 같이 보기 
[PHP에서 문자열을 다루는 다른 유용한 함수들](https://velog.io/@dcmnt/Different-Functions-in-PHP-For-Strings)