---
title:                "Fish Shell: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜

정규표현식을 사용하는 이유는 무엇일까요? 정규표현식을 사용하면 텍스트에서 원하는 패턴을 추출하고 변형할 수 있기 때문입니다. 예를 들어, 이메일 주소, 전화번호, 또는 우편번호와 같이 특정한 형식을 가진 텍스트를 찾는데 유용합니다. 또한, 반복적인 작업이나 대량의 데이터를 처리할 때도 정규표현식을 사용하면 효율적입니다.

## 사용 방법

정규표현식은 Fish 셸에서도 간단하게 사용할 수 있습니다. 아래 예제를 참고하여 정규표현식의 기본적인 사용법을 익혀보세요.

```Fish Shell
# 텍스트에서 이메일 주소 추출하기
echo "안녕하세요! 제 이메일 주소는 example@example.com입니다." | grep -oE '[[:alnum:]+\.\_\-]+@[[:alpha:]]+\.[[:alpha:]]+'

# 출력: example@example.com
```

## 더 깊게 파헤치기

정규표현식을 더 자세히 이해하기 위해서는 패턴을 구성하는 요소들에 대해 알아야 합니다. 아래 링크를 참고하여 보다 깊이있는 정보를 얻을 수 있습니다.

- [정규표현식 기본 지식](https://www.regular-expressions.info/)
- [Fish 셸에서 정규표현식 사용하기](https://fishshell.com/docs/current/cmds/pcre.html)
- [그 외 다양한 정규표현식 예제들](https://regexone.com/)

## 참고

- [Markdown 문법 가이드](https://www.markdownguide.org/basic-syntax)
- [Fish 셸 공식 홈페이지](https://fishshell.com/)
- [정규표현식에 대한 더 많은 정보](https://ko.wikipedia.org/wiki/%EC%A0%95%EA%B7%9C_%ED%91%9C%ED%98%84%EC%8B%9D)