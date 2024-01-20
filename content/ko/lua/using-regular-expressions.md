---
title:                "정규 표현식 사용하기"
html_title:           "Lua: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

Lua에서 정규식 사용하기

## 정규식이란 무엇이며 왜 사용할까요?
정규식은 문자열에서 특정한 패턴을 찾거나 대체하고자 할 때 사용하는 패턴 매칭 도구입니다. 이것은 프로그래머에게 강력한 문자열 처리를 제공하며, 특히 복잡한 문자열을 다룰 때 유용합니다.

## 사용 방법:
Lua에서 정규식을 사용하려면, 먼저 ```Lua``` 코드 블록 안에 ```string``` 라이브러리의 ```match()``` 함수를 사용해야 합니다. 아래 예제를 참조하세요.

```
-- 문자열에서 'hello'를 찾아 출력하기
local str = "안녕하세요, hello, こんにちは"
print(str:match("hello")) -- output: hello

-- 패턴 매칭을 이용해 일본어 문장에서 한국어 문장 추출하기
local sentence = "すみません、こんにちは、ごめんなさい"
print(sentence:match("こんにちは、(.*)")) -- output: ごめんなさい
```

## 깊게 들어가보기:
정규식은 1950년대 부터 사용되어 온 기술로, 프로그래밍에서 적극적으로 활용됩니다. 다른 대안으로는 정규식 대신 문자열의 일부를 직접 추출하는 방법이 있지만, 정규식을 사용하는 것이 더 유연하고 효율적입니다. Lua에서는 정규식 엔진으로 PCRE(Perl 호환)를 사용하며, 특수한 문자 패턴을 표현하기 위해 이스케이프 문자를 사용할 수 있습니다.

## 관련 자료:
- [Lua string 라이브러리 문서](http://lua-users.org/wiki/StringLibraryTutorial)
- [PCRE 패턴 문법 가이드](http://www.lua.org/manual/5.3/manual.html#6.4.2)