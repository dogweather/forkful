---
title:    "PHP: 문자열의 길이 찾기"
keywords: ["PHP"]
---

{{< edit_this_page >}}

# 왜

문자열의 길이를 찾는 작업은 프로그래밍에서 매우 중요합니다. 이를 통해 입력 값의 유효성을 검증하고, 문자열을 자르거나 조작하는 등 여러 가지 작업을 수행할 수 있습니다. 따라서 프로그래밍을 하는 데 있어서 문자열의 길이를 찾는 것은 필수적인 작업이라고 할 수 있습니다.

# 방법

PHP를 이용하여 문자열의 길이를 찾는 방법은 매우 간단합니다. 해당 문자열 변수 뒤에 ```strlen()``` 함수를 이용하여 호출하면 됩니다. 아래는 기본적인 예제 코드입니다.

```PHP
$str = "안녕하세요";
echo strlen($str);
```

위 코드의 출력 결과는 ```6```이 됩니다. 이는 한글의 경우 한 글자당 2바이트로 인식하기 때문입니다. 만약 영문자만 있는 경우라면 한 글자당 1바이트로 인식되므로 출력 결과 역시 해당하는 문자열의 길이가 됩니다.

# 깊이 들어가기

문자열의 길이를 찾는 과정에서는 문자의 인코딩 방식에 따라 다른 결과가 나오는 경우가 있습니다. 위 예제에서는 UTF-8 인코딩 방식을 기준으로 한글의 경우 2바이트로 처리하였지만, 다른 인코딩 방식일 경우 결과가 다를 수 있습니다. 따라서 다양한 인코딩 방식에 따른 출력 결과를 고려하여 프로그래밍하시길 권장합니다.

# 참고

* [PHP strlen() 문서](https://www.php.net/manual/en/function.strlen.php)
* [문자열의 길이 찾기와 바이트 수 세기](https://helloworldmachines.tistory.com/273)