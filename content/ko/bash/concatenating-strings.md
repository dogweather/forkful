---
title:                "문자열 연결하기"
html_title:           "Bash: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜
문자열을 연결하는 이유는 간단합니다. 여러 개의 문자열을 하나로 합쳐서 보기 쉬운 형태로 만들기 위해서입니다.

## 방법
간단한 예제를 통해 Bash에서 문자열을 연결하는 방법을 알아보겠습니다. 아래의 코드를 터미널에 입력해보세요.

```Bash
# 3개의 문자열을 변수에 저장합니다.
name="Jane"
age="25"
hobby="reading"

# 변수들을 연결하여 새로운 문자열을 만듭니다.
intro="$name is $age years old and enjoys $hobby."

# 출력해보면 `Jane is 25 years old and enjoys reading.`가 나옵니다.
echo $intro
```

## 깊게 들어가기
Bash에서는 문자열을 연결할 때 변수를 사용하여 자유롭게 수정할 수 있습니다. 예를 들어, 위의 예제에서 `age` 변수를 변경하면 새로운 나이로 자동으로 업데이트됩니다. 또한, 여러 개의 변수를 한 번에 연결하는 것도 가능합니다.

## 관련 자료
- [Bash string concat - Stack Overflow](https://stackoverflow.com/questions/4181703/how-to-concatenate-string-variables-in-bash)
- [How to concatenate strings in Bash - Linuxize](https://linuxize.com/post/how-to-concatenate-strings-in-bash/)