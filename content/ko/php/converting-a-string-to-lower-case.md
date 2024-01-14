---
title:    "PHP: 문자열을 소문자로 변환하기"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것에 관심을 가지는 이유는 무엇일까요? 대부분의 경우, 문자열에 대소문자가 섞여 있을 때 특정 작업을 수행하기 위해서입니다.

## 하는 법

다행히도, PHP에서 문자열을 소문자로 변환하는 것은 매우 간단합니다. 다음 코드를 사용하여 간단한 예제를 확인해보세요.

```PHP
$str = "Hello World!";
echo strtolower($str);
```

출력:
```PHP
hello world!
```

위 예제에서 우리는 `strtolower()` 함수를 사용하여 `$str` 변수의 값인 "Hello World!"를 소문자로 변환하고 출력하였습니다. 이제 문자열이 모두 소문자가 되었음을 알 수 있습니다.

## 깊게 파헤치기

PHP에서 문자열을 소문자로 변환하는 방법은 실제로 더 깊게 들어가면 더 복잡해질 수 있습니다. 예를 들어, 다른 언어나 문자열을 사용하면 변환이 올바르게 이루어지지 않을 수 있습니다. 이 경우, `setlocale()` 함수를 사용하여 올바른 로케일 값을 설정해야 할 수도 있습니다.

또 다른 중요한 포인트는 문자열이 유니코드인지 아닌지에 따라 변환이 다를 수 있다는 것입니다. 일부 언어에서는 대소문자에 대한 정확한 구별이 필요하므로, `strtolower()` 함수를 사용하기 전에 문자열 첫 번째 인덱스에서 문자 맥락을 확인하는 것이 중요합니다.

## 미리 보기

우리는 지금 문자열을 소문자로 변환하는 방법을 미리 보았지만, PHP 공식 문서에서 더 많은 정보를 찾을 수 있습니다. [PHP 문자열 함수](https://www.php.net/manual/en/ref.strings.php) 페이지를 방문하여 더 많은 예제와 설명을 확인해보세요.

## 연관 자료

- [PHP 문자열 함수 공식 문서](https://www.php.net/manual/en/ref.strings.php)
- [PHP `strtolower()` 함수 공식 문서](https://www.php.net/manual/en/function.strtolower.php)