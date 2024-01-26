---
title:                "문자열에서 따옴표 제거하기"
date:                  2024-01-26T03:40:54.800432-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열에서 따옴표 제거하기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열에서 따옴표를 제거한다는 것은 텍스트를 감싸고 있는 그 더블 또는 싱글 따옴표 문자를 벗겨내는 것을 의미합니다. 프로그래머들은 입력 값을 정화하거나, 파싱을 용이하게 하거나, 일관성이 없게 인용된 데이터를 조화롭게 만들기 위해 이 작업을 합니다.

## 어떻게:
Lua에서 따옴표를 제거하는 방법은 여기 있습니다:

```lua
local function remove_quotes(str)
  return (str:gsub("^%p(.*)%p$", "%1"))
end

print(remove_quotes('"안녕, 세상!"'))     -- 안녕, 세상!
print(remove_quotes("'안녕히 가세요, 따옴표!'"))  -- 안녕히 가세요, 따옴표!
```

빙고! 그 따옴표들은 마치 건조기 속 양말처럼 사라졌습니다.

## 심층 분석
언어가 텍스트를 처리할 수 있게 된 이래로 사람들은 문자열에서 따옴표를 제거해 왔습니다, 그것은 거의 영원한 시간 동안의 일입니다. Lua에서는 `gsub` 함수가 무거운 짐을 들어, 패턴을 메스처럼 사용해 따옴표를 제거합니다. 대안? 물론, 지원하는 언어에서는 정규 표현식을 사용하거나, 각 문자를 통해 실행하는 자신만의 루프를 작성할 수 있습니다(하품, 하지만 어쨌든 그것은 당신의 시간입니다).

Lua의 패턴 매칭은 전체 라이브러리를 가져오지 않고도 정규 표현식 경험의 영향을 줍니다. 캐럿(`^`)과 달러 기호(`$`)는 각각 문자열의 시작과 끝을 일치시킵니다; `%p`는 모든 구두점 문자와 일치합니다. 맨 앞과 맨 뒤의 구두점을 털어낸 후, `(.*),`로 모든 것을 캡처하고 `" %1"`을 사용하여 전체 일치를 그 캡처 그룹으로 교체합니다.

Lua의 패턴 매칭이 전체 정규 표현식 엔진만큼 강력하지 않다는 것을 기억하세요 - 예를 들어, 개수를 세거나 되감기(backtrack)를 할 수 없습니다. 이러한 단순함은 당신이 다루고 있는 따옴표와 그것들이 숨어 있는 위치에 따라 축복일 수도 있고 저주일 수도 있습니다.

## 또한 보기
Lua의 패턴 매칭에 대해 더 깊이 파고들어 보세요, PiL(Programming in Lua) 책에서: http://www.lua.org/pil/20.2.html

기교 넘치는 방법을 확인하고 싶다면, 비교를 위해 다른 언어가 이를 어떻게 하는지 확인하세요, Python의 `str.strip`부터 시작하세요: https://docs.python.org/3/library/stdtypes.html#str.strip