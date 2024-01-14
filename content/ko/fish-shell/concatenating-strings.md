---
title:                "Fish Shell: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜: 문자열을 연결하는 일을 하려면

사람들이 문자열을 연결하는 일을 하는 이유는 무엇일까요? 바로 두 가지 이유 때문입니다. 첫째, 여러 개의 문자열을 한 번에 출력하고 싶을 때 유용합니다. 둘째, 연결된 문자열은 보다 의미 있는 데이터를 가지고 있기 때문에 데이터 처리에 유용합니다.

## 방법: Fish 셸에서 문자열 연결하는 방법

Fish 셸에서 문자열을 연결하는 방법에는 두 가지 방법이 있습니다. 첫번째는 `string join` 명령어를 사용하는 것입니다. 두 번째는 `set` 명령어를 사용하여 변수를 설정하는 것입니다.

```
Fish Shell에서 문자열 연결하는 방법 예제:

string join "안녕하세요," "제 이름은" "길동이에요" 
# => 안녕하세요, 제 이름은 길동이에요

set name "길동이"
echo "제 이름은" $name "입니다."
# => 제 이름은 길동이입니다.
```

## 깊이 들어가기: 문자열 연결에 대해 알아보기

문자열을 연결하는데 사용되는 `string join` 명령어는 여러 개의 문자열을 받아 하나의 문자열로 결합합니다. 이 때, 첫 번째 매개변수는 분리자(delimiter)로 사용됩니다. 분리자는 각 문자열 사이에 추가되어 서로 구분할 수 있도록 돕습니다.

`set` 명령어를 사용하여 변수를 설정하려면 변수 이름 뒤에 `=`를 붙이고 값을 설정하면 됩니다. 값 앞에 `$`를 붙이면 변수의 값이 사용되는 것을 나타낼 수 있습니다.

## 참고: 

- [Fish 셸 가이드 - 문자열 연결](https://fishshell.com/docs/current/index.html#string-subcommands)
- [Fish 셸 유용한 명령어 5가지](https://behinder.tistory.com/188)