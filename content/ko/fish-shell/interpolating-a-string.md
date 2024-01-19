---
title:                "문자열 보간하기"
html_title:           "Java: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 뭐고 왜하나?
문자열을 보간하는 것은 일련의 문자열 내에 변수 값을 삽입하는 방법입니다. 이는 출력을 동적으로 조정하거나 문자열을 사용자 정의 할 수 있게 해 줍니다.

## 어떻게 하는가:
Fish Shell에서 문자열을 보간하려면 변수 이름을 비롯한 구문을 중괄호 {}로 구분해야 합니다. 아래 코드를 참조하세요.

```Fish Shell
set name "Fish"
echo "Hello, {$name} Shell!"
```

결과:

```Fish Shell
Hello, Fish Shell!
```

## 깊이 들여다 보기
수그레기(shell) 스크립트에서 문자열 보간은 오래 전부터 흔히 사용되는 기능입니다. Bash, KornShell 등에서도 유사하게 사용됩니다. 하지만 Fish에서는 문자열 보간에 중괄호 {}를 사용합니다.

또한, `printf` 펑션을 이용한 알려진 대안 방법이 있습니다. 다음 코드 블록은 이 부분을 보여줍니다.

```Fish Shell
set name "Fish"
printf "Hello, %s Shell!\n" $name
```

Fish Shell은 변수 이름에 중괄호{}를 사용하는 것으로 문자열 보간을 쉽게 식별할 수 있게 만듭니다.

## 참고 자료
Fish Shell 공식 문서:
- [설정 변수 설정 ](https://fishshell.com/docs/current/cmds/set.html)
- [내장 printf 명령어 설명](https://fishshell.com/docs/current/cmds/printf.html)
- [String Interpolation](https://fishshell.com/docs/current/tutorial.html#tut_escaping)