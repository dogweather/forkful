---
title:    "Fish Shell: '정규식 사용하기'"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜 이용해야 하는가 

정규식은 문자열을 다루는데에 있어서 매우 유용합니다. 예를 들어, 주민등록번호나 이메일 주소와 같은 특정한 패턴을 가지고 있는 문자열을 찾을 때 정규식을 사용할 수 있습니다. 또한, 대용량의 텍스트 파일에서 원하는 정보를 빠르게 추출할 수 있어서 효율적인 데이터 처리를 할 수 있습니다.

## 사용 방법

정보를 추출할 때는 마침표(.)와 별표(*)를 주로 사용합니다. 마침표는 한 글자를 나타내고, 별표는 0개 이상의 글자를 나타냅니다. 예를 들어, 아래와 같은 명령어는 정규식을 사용하여 이메일 주소를 찾아 출력합니다.

```Fish Shell
grep -E "\w+@\w+\.\w+" email_list.txt
```

위의 명령어는 "email_list.txt" 파일에서 이메일 주소를 추출하여 출력합니다. 정규식 \w+는 한 글자 이상의 단어 또는 숫자를 의미하고, @는 이메일 주소의 구분자를 나타냅니다.

## 깊이 알아보기

위의 예시처럼 간단한 정규식만으로도 정보를 추출할 수 있지만, 더 복잡한 패턴을 찾을 때에는 여러가지 특수 문자를 사용할 수 있습니다. 예를 들어, 정규식에서 괄호를 사용하여 그룹으로 묶은 뒤, 그룹을 반복할 수 있습니다. 또한, 반복되는 문자를 찾을 때에는 중괄호를 사용하여 횟수를 지정할 수 있습니다.

더 많은 정보와 예제는 [정규 표현식 레퍼런스](https://fishshell.com/docs/current/cmds/grep.html#-e)를 참고하시기 바랍니다.

## 관련 자료

- [정규 표현식 가이드](https://regexone.com)
- [정규 표현식 연습 사이트](https://regexr.com)
- [정규 표현식을 사용한 문자열 검색 예제](https://fishshell.com/docs/current/cmds/grep.html#-e)