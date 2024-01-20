---
title:                "문자열의 길이 찾기"
html_title:           "Lua: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

문자열 길이 찾기란 요소의 수량을 카운트하는 것입니다. 개발자들은 자료 처리, 입력 유효성 검사, 배열 채우기 등의 상황에서 이 기능을 사용합니다.

## 어떻게 실행하나요:

아래는 주요 코드 예제와 출력입니다.

```Lua
s = "안녕하세요, 루아!"
print(#s)
```

위 코드를 실행하면, 결과는 '13'입니다.

## 깊게 들어가기:

1. **역사적 맥락**: 루아는 길이 연산자가 포함된 최초의 몇 가지 프로그래밍 언어 중 하나로서, 이를 문자열 처리에 활용할 수 있습니다.

2. **대안**: 'string.len' 함수도 문자열 길이를 찾는 또 다른 방법입니다. 예를 들면:

    ```Lua
    s = "안녕하세요, 루아!"
    print(string.len(s))  
    ```

    위 코드를 실행하면, 결과 역시 '13'입니다.

3. **작동 세부 정보**: # 연산자는 문자열의 내부 표현에 따라 효율적으로 작동합니다. utf-8 기반 문자열에서 한글은 각 3 바이트로 계산됩니다. 따라서 실제 문자 수보다 결과 값이 더 클 수 있습니다.

## 참고 자료:

1. [Lua String 튜토리얼](https://www.tutorialspoint.com/lua/lua_strings.htm)
2. [Lua API: 문자열 기능](https://www.lua.org/pil/20.html)
3. [루아: # 연산자](https://www.lua.org/manual/5.3/manual.html#3.4.7)