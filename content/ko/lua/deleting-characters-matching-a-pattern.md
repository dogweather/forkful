---
title:                "패턴에 일치하는 문자 삭제"
html_title:           "Fish Shell: 패턴에 일치하는 문자 삭제"
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

패턴에 일치하는 문자를 삭제하는 것은, 문자열에서 특정 문자 또는 문자의 시퀀스를 제거하는 프로세스를 말합니다. 이는 데이터 정리, 형식 일관성 유지, 불필요한 문자 제거 등의 작업을 달성하기 위해 프로그래머들이 사용합니다.

## 어떻게 하나요?
다음은 Lua에서 패턴이 일치하는 문자를 삭제하는 코드 예시입니다.

```Lua
s = "Hello, World!!"
r = s:gsub("%p", "")
print(r)
```
이 코드는 모든 구두점을 삭제합니다. 출력 결과는 다음과 같습니다.

```Lua
Hello World
```

## 깊게 알아보기

Lua의 `gsub` 함수는 Perl에서 영감을 받아 만들어졌습니다. 이는 Perl의 강력한 정규 표현식 처리 능력에 착안한 것입니다. 

다른 언어에서도 비슷한 기능을 찾을 수 있는데, 예를 들면 Python에서는 `re.sub()` 함수, JavaScript에서는 `replace()` 메소드를 이용합니다.

`gsub` 함수는 기본적으로 문자열을 스캔하여 주어진 패턴에 일치하는 모든 부분을 찾습니다. 그리고 찾은 부분을 두 번째 인수로 제공된 값으로 바꿉니다. 일치 항목을 삭제하려면, 대체 문자열로 빈 문자열(`""`)을 사용하면 됩니다.

## 참고 자료

- Lua 문자열 라이브러리: https://www.lua.org/pil/20.html
- Lua 패턴 매칭: https://www.lua.org/pil/20.2.html
- Perl의 Strings: https://perldoc.perl.org/functions/substr.html
- Python의 re.sub(): https://docs.python.org/3/library/re.html#re.sub
- JavaScript의 replace(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace