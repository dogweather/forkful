---
title:    "Fish Shell: 문자열을 소문자로 변환하기"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜
문자열을 소문자로 변환하는 것의 장점은 문자열을 일관되게 처리할 수 있다는 것입니다.

## 방법
우리는 Fish Shell의 내장 함수인 `string tolower`을 사용하여 문자열을 소문자로 변환할 수 있습니다.
```
Fish Shell> echo "HELLO" | string tolower
hello
```
위의 예시와 같이 `echo` 명령어를 사용하여 "HELLO"라는 문자열을 출력하고, `string tolower` 함수를 사용하여 소문자로 변환합니다.
또는, `string tolower` 함수를 변수에 할당하여 사용할 수도 있습니다.
```
Fish Shell> set myString "WORLD"
Fish Shell> set lowercaseString (string tolower $myString)
Fish Shell> echo $lowercaseString
world 
```

## 딥 다이브
`string tolower` 함수는 문자열을 소문자로 변환하는 데에 있어서 유용하지만, 주의해야 할 점도 있습니다.
예를 들어, `string tolower` 함수는 단어의 첫 글자만 소문자로 변환하므로 전체 문자열이 소문자로 변환되지는 않습니다.
또한,`string tolower` 함수는 UTF-8 문자를 올바르게 처리하지 못할 수도 있습니다.
따라서 UTF-8 문자열을 다룰 때는 `string tolower --universal` 옵션을 사용하여 모든 문자를 소문자로 변환하도록 해야 합니다.

## 참고
* [Fish Shell 공식 문서](https://fishshell.com/docs/current/cmds/tolower.html)
* [사용자 커뮤니티에서 제공하는 유용한 팁](https://github.com/fish-shell/fish-shell/wiki/Useful-Functions-and-Snippets)