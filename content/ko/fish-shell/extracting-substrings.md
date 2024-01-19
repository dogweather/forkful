---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 대체 무엇이고 왜 필요한가요?

문자열 추출이란 주어진 문자열에서 특정 부분을 선택해 가져오는 것 입니다. 이것은 데이터 분석, 텍스트 조작 및 검색과 같은 다양한 프로그래밍 상황에서 유용하게 사용됩니다.

## 어떻게 하나요:
다음 코드는 "Fish Shell"에서 어떻게 서브 스트링을 추출하는지 보여줍니다.

```Fish Shell
set str "Hello, Fish Shell"
echo (string sub -s 2 -l 5 $str)
```
이 코드를 실행하면 "ello," 부분이 출력됩니다.

```Fish Shell
ello,
```

## Deep Dive:
문자열 추출은 프로그래밍의 매우 중요한 부분으로, 문자열을 효과적으로 조작하고 특정 정보를 획득하는 데 사용됩니다. 'Fish Shell'에서는 built-in 'string' 명령어를 사용하여 간단하게 문자열을 조작할 수 있습니다. 또한 다른 대안으로 awk, sed 등의 도구를 사용할 수 있지만 'string' 명령어가 더 직관적이며 사용하기 쉽습니다. 

위에서 언급한 코드의 첫 번째 매개변수 '-s'는 시작 인덱스를, 두 번째 매개변수 '-l'는 길이를 나타내며, 이 두 매개변수를 사용하여 어떤 문자열에서든 원하는 서브 스트링을 추출할 수 있습니다.

## 추가 정보:
다음은 관련된 소스들입니다:
- [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
- [Fish Shell 문자열 조작에 대한 스택오버플로우 답변](https://stackoverflow.com/questions/6525051/how-can-i-extract-a-substring-in-fish-shell)