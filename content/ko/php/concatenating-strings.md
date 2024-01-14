---
title:                "PHP: 문자열 연결하기"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/concatenating-strings.md"
---

{{< edit_this_page >}}

# 왜 스트링 합성을 사용해야 할까요?

일반적으로 프로그래밍에서 스트링 합성은 컴퓨터가 문자열을 조합하여 새로운 문자열을 만들어내는 과정을 의미합니다. 이는 텍스트 처리 및 문자열 조작에 매우 유용하며, PHP에서도 이와 관련된 여러 기능을 제공합니다. 스트링 합성을 사용하면 프로그래머는 보다 간편하게 문자열을 다룰 수 있고 원하는 결과를 더 빠르게 얻을 수 있습니다.

## 사용 방법

스페이스를 추가하고 싶은 경우, 아래와 같이 `.` 연산자를 이용하여 두 개의 문자열을 합칠 수 있습니다.

```PHP
echo "안녕" . "하세요";
```

출력 결과는 `안녕하세요`가 됩니다. 또한, 변수와 문자열을 함께 사용할 수도 있습니다.
```PHP
$name = "철수";
echo $name . "의 나이는 20살 입니다.";
```

위 코드의 출력 결과는 `철수의 나이는 20살 입니다`가 됩니다. 또한, PHP에서는 여러 문자열을 한 번에 합성하는 방법도 제공됩니다. 아래와 같이 `UseImplode()` 함수를 사용하여 배열의 문자열들을 합칠 수 있습니다.

```PHP
$animals = array("고양이", "강아지", "토끼");
echo implode(", ", $animals);
```

출력 결과는 `고양이, 강아지, 토끼`가 됩니다. 또한, 문자열 안에 변수를 포함할 때는 다음과 같이 `{}`로 감싸야 합니다.

```PHP
$weather = "흐림";
echo "오늘 날씨는 {$weather}입니다.";
```

출력 결과는 `오늘 날씨는 흐림입니다.`가 됩니다.

## 더 깊게 들어가보기

PHP에서는 보다 복잡한 문자열 합성을 위해 `str_replace()` 함수를 제공합니다. 이 함수를 사용하면 문자열에서 특정 패턴을 찾아서 다른 패턴으로 바꿀 수 있습니다. 예를 들어, 다음과 같이 이름을 가진 사람에게 인사하는 메시지를 만들 수 있습니다.

```PHP
$name = "영희";
$message = "안녕, {name}야. 뭐하고 지내?";
$final_message = str_replace("{name}", $name, $message);
```

위 예제에서 `str_replace()` 함수는 `$message` 변수에서 `{name}`이라는 패턴을 `$name` 변수의 값으로 바꾸어 `$final_message` 변수에 저장합니다. 출력 결과는 `안녕, 영희야. 뭐하고 지내?`가 됩니다.

# See Also

- [PHP 문자열 합성 관련 문서](https://www.php.net/manual/en/language.operators.string.php)
- [PHP 문자열 함수들](https://www.php.net/manual/en/ref.strings.php)