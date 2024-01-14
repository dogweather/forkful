---
title:                "PHP: 문자열 대문자로 바꾸기"
simple_title:         "문자열 대문자로 바꾸기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

스트링의 첫 글자를 대문자로 바꾸는 것의 이유는 뭘까요? 글자의 첫 글자를 대문자로 변환하는 것은 우리가 일상 생활에서 자주 하는 작업입니다. 예를 들어, 이름을 입력할 때나 문장의 첫 글자를 대문자로 시작할 때 우리는 이 작업을 반복적으로 수행합니다. 하지만 이번에는 이 작업을 컴퓨터 프로그래밍에서 어떻게 수행할 수 있는지 알아보겠습니다.

## 하는 방법

PHP에서 스트링의 첫 글자를 대문자로 바꾸는 방법은 다양합니다. 가장 간단한 방법은 `ucfirst()` 함수를 이용하는 것입니다. 이 함수는 주어진 스트링의 첫 글자를 대문자로 변환하여 반환합니다. 다음은 이 함수의 사용 예시입니다.

```PHP
$name = "kayla";
echo ucfirst($name); // 출력결과: Kayla
```

하지만 만약 이름이 여러 개 들어있는 스트링을 한 번에 처리하고 싶다면 어떻게 해야할까요? 이때는 `explode()` 함수를 이용하여 스트링을 배열로 변환한 후, `array_map()` 함수를 이용하여 각 요소의 첫 글자를 대문자로 변환하는 방법이 있습니다. 다음은 이 방법의 코드 예시입니다.

```PHP
$names = "kayla, sam, jason";
$nameArray = explode(", ", $names);
$result = array_map('ucfirst', $nameArray);
echo implode(", ", $result); // 출력결과: Kayla, Sam, Jason
```

## 깊이 파고들기

스트링의 첫 글자를 대문자로 바꾸는 데에는 많은 방법이 있지만, 이중에서도 가장 중요한 것은 바로 변환할 스트링의 언어를 고려하는 것입니다. 만약 영어로 작성된 스트링을 처리하는 경우에는 위에서 언급한 방법들이 문제없이 동작하지만, 다른 언어일 경우에는 조금 다른 방식으로 접근해야 할 수도 있습니다. 예를 들어 일본어와 같은 언어의 경우에는 글자 하나가 아닌 음절 단위로 대문자가 적용되는 경우도 있습니다.

따라서 만약 일반적이지 않은 언어를 다루는 경우에는 해당 언어의 특성을 고려하여 첫 글자를 대문자로 변환하는 로직을 작성해야합니다. 또한, 유니코드 문자를 다루는 경우에도 주의해야 합니다. UTF-8 인코딩을 사용하는 언어의 경우에는 해당 문자가 유니코드 문자인지를 반드시 확인한 후 대문자로 변환해야 합니다.

## 참고자료

- [PHP 공식 문서 - ucfirst() 함수](https://www.php.net/manual/en/function.ucfirst.php)
- [PHP 공식 문서 - explode() 함수](https://www.php.net/manual/en/function.explode.php)
- [PHP 공식 문서 - array_map() 함수](https://www.php.net/manual/en/function.array-map.php)