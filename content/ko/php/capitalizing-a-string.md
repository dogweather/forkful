---
title:                "PHP: 문자열 대문자로 변경하기"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 왜 문자열을 대문자로 변환해야 할까요?

문자열을 대문자로 변환하는 것은 많은 이유가 있습니다. 예를 들어, 데이터베이스에서 사용자의 이름을 저장할 때 대부분의 경우 대문자로 저장을 해야 합니다. 이렇게 하면 검색이나 비교할 때 더 효율적이기 때문입니다. 또한, 사용자가 입력한 데이터를 대소문자를 구분하지 않고 사용하도록 할 때도 대문자로 변환하는 것이 좋습니다. 

# 대문자로 변환하는 방법은?

만약 PHP로 프로그래밍을 한다면, 기본적으로 제공되는 함수인 `strtoupper()`를 사용하면 됩니다. 이 함수는 문자열을 모두 대문자로 변환해줍니다. 아래는 `strtoupper()` 함수를 사용하는 예제 코드와 출력 결과입니다.

```PHP
<?php
$string = "hello world";
echo strtoupper($string); // HELLO WORLD
?>
```

만약 서버 측에서 사용자가 입력한 문자열을 대문자로 변환한 후 데이터베이스에 저장하려면, 아래와 같이 사용할 수 있습니다.

```PHP
<?php
$name = $_POST["name"];
$name = strtoupper($name); // 사용자가 입력한 이름을 대문자로 변환
// 데이터베이스에 $name 값 저장
?>
```

# 깊게 들어가보기

`strtoupper()` 함수는 단순히 문자열을 대문자로 변환하는 것 보다 조금 더 깊게 들어가보겠습니다. 문자열에는 다양한 언어의 문자들이 포함될 수 있기 때문에, 문자열을 대문자로 변환할 때는 이를 고려해야 합니다. 예를 들어 알파벳 외에도 공백, 숫자, 특수문자, 한글 등이 존재합니다. 또한, 일부 언어에서는 문자를 대문자로 변환할 때 발음이나 억양이 바뀌는 경우도 있습니다. 따라서 모든 언어에 대해 일관된 대문자 변환이 이루어져야 합니다. 

또 다른 주의할 점은 대문자 변환 후 문자열의 길이가 바뀔 수 있다는 것입니다. 예를 들어 소문자 'i'는 대문자로 변환하면 'I'가 되지만, 한글은 대문자로 변환해도 길이가 바뀌지는 않습니다. 이런 경우, 변환 후의 길이를 체크하여 코드를 작성해야 합니다. 

이처럼 대문자로 변환하는 것은 간단해 보이지만, 다양한 언어의 문자들을 모두 고려하여 일관성 있게 처리해야 하기 때문에 조금 깊게 공부해야 합니다. 

# 참고하기

- [PHP `strtoupper()` 문서](https://www.php.net/manual/kr/function.strtoupper.php)
- [왜 문자열 처리는 중요할까?](https://brunch.co.kr/@jeffreymo/7)
- [Case Folding in Unicode](https://unicode.org/faq/casemap_charprop.html) (영문)