---
title:                "Bash: 문자열 소문자로 변환하기"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜 

만약 당신이 텍스트를 처리하는 프로그램을 작성한다면, 입력되는 문자열을 소문자로 바꿔야 할 필요가 있을 수 있습니다. 소문자 문자열은 사용자들이 읽기 쉽고, 문자열을 비교하는데 유용합니다. 이제 소문자로 바꾸는 방법을 알아보도록 하겠습니다. 

## 어떻게 

여러 가지 언어에는 문자열을 소문자로 바꾸는 내장 함수가 있지만, Bash에는 내장 함수가 없습니다. 그러나 우리는 `tr` 명령어를 사용하여 문자열을 소문자로 바꿀 수 있습니다. `tr` 명령어는 특정 문자를 다른 문자로 변환시키는 데 사용됩니다. 이 명령어를 사용하면 입력된 문자열에 있는 모든 대문자 문자를 소문자로 바꿀 수 있습니다. 

아래는 `tr` 명령어를 사용하여 문자열을 소문자로 바꾸는 간단한 예제입니다. 

``Bash
echo "HELLO WORLD" | tr '[:upper:]' '[:lower:]'
``

*출력: hello world*

`echo` 명령어를 사용하여 "HELLO WORLD"라는 문자열을 출력하고, `tr` 명령어를 사용하여 대문자를 소문자로 바꿉니다. 

이제 여러분은 간단한 `tr` 명령어를 사용하여 문자열을 소문자로 바꿀 수 있습니다. 하지만 더 많은 옵션을 사용하여 조금 더 세부적으로 변환할 수 있습니다. 

## 깊이 알아보기 

`tr` 명령어에 대한 더 자세한 정보를 알고 싶다면, `man` 페이지를 확인할 수 있습니다. 

``Bash
man tr
``

`man` 페이지를 확인하면 `tr` 명령어 옵션에 대한 자세한 설명을 볼 수 있습니다. 예를 들어, 우리가 사용한 `[:upper:]`과 `[:lower:]` 옵션 외에도 `tr` 명령어에는 문자를 제거하거나 변환하는 다른 유용한 옵션이 있습니다. 

또한 `tr` 명령어의 `[:upper:]`와 `[:lower:]` 옵션은 만약 입력된 문자열에 대문자가 없더라도 동작하게 됩니다. 이 옵션들은 입력된 문자열에 있는 모든 문자를 대체하지는 않고, 대문자 영역만 바꾸기 때문입니다. 

`tr` 명령어를 사용하여 문자열을 소문자로 바꾸는 것은 매우 간단하지만, 몇 가지 더 복잡한 옵션을 사용하여 더 많은 기능을 수행할 수 있습니다. 자세한 정보는 `man` 페이지를 참고하시기 바랍니다! 

## 관련 링크 

- [Bash 코드 블록 사용하기](https://gist.github.com/roachhd/1f029bd4b50b8a524f3c)
- [Bash 사용 팁 및 트릭](https://www.shellscript.sh/tips/)
- [Bash 공식 문서](https://www.gnu.org/software/bash/manual/)
- [문자열 처리 및 형식 지정](http://tldp.org/LDP/abs/html/string-manipulation.html)

## 참고하기

- [Bash의 tr 명령어](https://www.computerhope.com/unix/utr.htm)
- [Linux용 tr 명령어 사용법](https://zetawiki.com/wiki/Linux_tr_%EB%AA%85%EB%A0%B9%EC%96%B4_%EC%82%AC%EC%