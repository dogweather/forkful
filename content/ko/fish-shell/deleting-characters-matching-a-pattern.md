---
title:                "패턴에 일치하는 문자 삭제"
html_title:           "Fish Shell: 패턴에 일치하는 문자 삭제"
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇인가요 & 왜 그러나요?
패턴과 일치하는 문자를 삭제하는 것은 특정 조건에 따라 문자열에서 문자를 제거하는 프로그래밍 기법입니다. 프로그래머들이 이것을 사용하는 이유는 코드를 정리하거나 불필요한 정보를 제거하기 위해서입니다.

## 어떻게 해야하나요:
Fish Shell에서 패턴을 이용한 문자 삭제는 string 명령어를 이용합니다. 예시를 통해 살펴봅시다.
```fish
# 변수 선언
set str "Hello, World!"

# 'l' 문자 삭제
set result (string replace -r 'l' '' -- $str)
echo $result
```
위 코드를 실행시키면 `l`이라는 문자가 삭제된 `Heo, Word!`라는 결과가 나옵니다.

## 깊이 들어가보기
문자의 패턴 삭제는 Unix Shell 프로그래밍에서 오래전부터 있었던 기능 중 하나입니다. 이 기능은 특정 문자나 패턴을 찾아서 다른 문자 또는 패턴으로 교체하거나 완전히 삭제하는 데 사용됩니다. Fish Shell도 이 기능을 string 명령어를 통해 제공하고 있습니다. 대안으로는 텍스트 처리 도구인 awk, sed 등이 있습니다.

## 참고 자료
Fish Shell에 관한 더 깊은 이해를 위해 아래의 리소스를 참고하세요.
- [Fish Shell 공식문서](http://fishshell.com/docs/current/index.html)
- [Fish Shell string 명령어 사용방법](http://fishshell.com/docs/current/cmds/string.html)