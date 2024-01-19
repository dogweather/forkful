---
title:                "문자열 보간하기"
html_title:           "Java: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

문자열 보간은 변수의 값을 문자열 내부에 삽입하는 방법입니다. 이 방법은 코드의 가독성을 향상시킵니다.

## 사용 방법:

Bash에서는 양 따옴표 내에 변수를 넣어 문자열 보간을 합니다. 
예를 보면:

```Bash
name="World"
echo "Hello, $name"
```
출력은 다음과 같습니다:

```
Hello, World
```

## 깊게 들어가기

Bash는 나열되는 많은 셀 스크립트 언어 중 하나로, 문자열 보간 기능은 원래 Bourne shell에서 나온 것입니다. 또한 Python이나 JavaScript 같은 다른 프로그래밍 언어에서도 문자열 보간을 사용할 수 있습니다. Bash에서는 두 종류의 문자열, 즉 단일 따옴표 ' ' 와 이중 따옴표 " " 가 있습니다. 단일 따옴표는 문자열 안에 존재하는 모든 문자들을 그대로 두는 반면에, 이중 따옴표는 문자열 안의 특정 문자를 특수 문자로 취급합니다.

## 참고 자료

1. [Bash Beginners Guide](https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_03_03.html)
2. [Bash Scripting Tutorial](https://www.shellscript.sh/variables1.html)
3. [String Interpolation in Bash](https://linuxize.com/post/bash-variable-in-string/)