---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:05.721070-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: Lua\uC5D0\uC11C \uC5F0\uAD00 \uBC30\uC5F4\
  (\uB610\uB294 Lua\uC5D0\uC11C\uB294 \uD14C\uC774\uBE14\uC774\uB77C\uACE0 \uD568\
  )\uC744 \uB9CC\uB4DC\uB294 \uAC83\uC740 \uC9C1\uAD00\uC801\uC785\uB2C8\uB2E4. \uAE30\
  \uBCF8\uC801\uC778 \uC22B\uC790 \uC778\uB371\uC2A4\uB97C \uBC84\uB9AC\uACE0 \uC790\
  \uC2E0\uC774 \uC120\uD0DD\uD55C \uD0A4\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uC774\
  \uAC83\uC744 \uD655\uC778\uD574\uBCF4\uC138\uC694."
lastmod: '2024-03-13T22:44:55.407068-06:00'
model: gpt-4-0125-preview
summary: "Lua\uC5D0\uC11C \uC5F0\uAD00 \uBC30\uC5F4(\uB610\uB294 Lua\uC5D0\uC11C\uB294\
  \ \uD14C\uC774\uBE14\uC774\uB77C\uACE0 \uD568)\uC744 \uB9CC\uB4DC\uB294 \uAC83\uC740\
  \ \uC9C1\uAD00\uC801\uC785\uB2C8\uB2E4."
title: "\uC5F0\uAD00 \uBC30\uC5F4 \uC0AC\uC6A9\uD558\uAE30"
weight: 15
---

## 사용 방법:
Lua에서 연관 배열(또는 Lua에서는 테이블이라고 함)을 만드는 것은 직관적입니다. 기본적인 숫자 인덱스를 버리고 자신이 선택한 키를 사용합니다. 이것을 확인해보세요:

```Lua
-- 연관 배열 생성하기
userInfo = {
  name = "Jamie",
  occupation = "Adventurer",
  level = 42
}

-- 요소 접근하기
print(userInfo["name"]) -- Jamie 출력
print(userInfo.occupation) -- Adventurer 출력

-- 새로운 키-값 쌍 추가하기
userInfo["hobby"] = "Coding"
userInfo.favLang = "Lua"

-- 연관 배열 순회하기
for key, value in pairs(userInfo) do
  print(key .. ": " .. value)
end
```

출력:
```
Jamie
Adventurer
name: Jamie
occupation: Adventurer
level: 42
hobby: Coding
favLang: Lua
```

멋진 점이 무엇이냐고요? 데이터와 상호작용할 때 여러분에게 의미 있는 키를 사용하여 코드를 더 읽기 쉽고 유지보수하기 쉽게 만듭니다.

## 심층 탐구
Lua가 등장했을 때, 테이블을 만능 데이터 구조로 소개하여 개발자들이 데이터를 관리하는 방식을 혁신시켰습니다. 일부 언어에서 연관 배열과 배열이 별개의 엔티티인 것과 달리, Lua의 테이블은 이 둘 모두로 사용되어 데이터 구조를 단순화합니다.

Lua 테이블을 특히 강력하게 만드는 것은 그 유연성입니다. 하지만, 이 유연성은 효율성을 위해 더 전문화된 데이터 구조가 선호될 수 있는 큰 데이터셋에서 잠재적인 성능 문제를 초래할 수 있다는 비용을 수반합니다.

Lua는 링크드 리스트나 해시 맵 같은 더 전통적인 데이터 구조를 박스 밖에서 네이티브로 지원하지 않지만, 필요하다면 테이블을 사용해 이러한 구조를 구현할 수 있는 테이블 구조의 적응성은 의미합니다. 기억하세요: 큰 힘에는 큰 책임이 따릅니다. 코드의 성능과 가독성을 유지하기 위해 유연성을 현명하게 사용하세요.
