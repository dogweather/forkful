---
title:                "Bash: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜

정규 표현식을 사용하는 이유는 매우 간단합니다. 이것은 문자열 안에서 원하는 패턴을 검색하고 추출하기 위한 강력한 도구입니다.

## 사용법

정규 표현식은 Bash에서 강력하고 유용하게 사용될 수 있습니다. 예를 들어, 문자열에서 숫자만 추출하는 경우를 생각해보겠습니다. 다음과 같이 코드를 작성할 수 있습니다.

```Bash
str="hello123world"
echo $str | grep -o '[0-9]*'
```

이 코드의 출력은 `123`이 될 것입니다. `grep`은 입력된 문자열에서 정규 표현식 `[0-9]*`와 일치하는 부분을 찾습니다. 이 경우에는 숫자만을 찾도록 지정했습니다. `grep`은 `-o` 옵션을 통해 일치하는 부분만을 출력합니다.

또 다른 예시로, 이메일 주소만을 추출하는 경우를 생각해보겠습니다. 예를 들어 다음과 같은 문자열이 있다고 가정해봅시다.

```Bash
str="My email is john@example.com. Please contact me!"
```

이 경우에는 다음과 같이 코드를 작성할 수 있습니다.

```Bash
echo $str | grep -o '[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,4}'
```

출력은 `john@example.com`이 될 것입니다. 여기서도 마찬가지로 `grep`의 `-o` 옵션을 사용하여 일치하는 부분만을 출력하도록 지정했습니다. 이 정규 표현식은 이메일 주소의 패턴을 정확히 나타냅니다.

## 깊게 파고들기

정규 표현식을 사용할 때 깊이 파고들면서 더 많은 패턴을 익힐 수 있습니다. 예를 들어 `[a-zA-Z0-9._%+-]+`는 이메일의 사용자 이름에 해당하는 부분을 나타내며, `[a-zA-Z0-9.-]+\.[a-zA-Z]{2,4}`는 이메일의 도메인 부분을 나타냅니다. 이와 같은 패턴을 조합하여 다양한 문자열에서 원하는 정보를 추출할 수 있습니다.

또한 정규 표현식에는 다양한 메타 문자와 오류 처리 등 다양한 기능들이 있습니다. 이를 모두 익혀두면 더욱 정확하고 유연하게 정규 표현식을 사용할 수 있습니다.

## 또 다른 참고자료

- [Bash에서 정규 표현식 사용하기](https://ryanstutorials.net/sed.php)
- [Bash와 정규 표현식을 이용한 텍스트 처리 기술](https://www.slideshare.net/limitedning/bashsedawking)
- [정규 표현식 연습 사이트](https://regexr.com/)
- [Bash 공식 문서](https://www.gnu.org/software/bash/manual/bash.html#Pattern-Matching)

## 참고자료

- [Regular Expressions in Bash](https://ryanstutorials.net/sed.php)
- [Text Processing Techniques in Bash using Regular Expressions](https://www.slideshare.net/limitedning/bashsedawking)
- [Regular Expression Practice Site](https://regexr.com/)
- [Bash Official Documentation](https://www.gnu.org/software/bash/manual/bash.html#Pattern-Matching)