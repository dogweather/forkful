---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:05.721070-07:00
description: "Lua\uC5D0\uC11C \uC5F0\uAD00 \uBC30\uC5F4\uC740 \uB9C8\uCE58 \uB370\uC774\
  \uD130\uC758 \uBE44\uBC00 \uC545\uC218\uC640 \uAC19\uC2B5\uB2C8\uB2E4. \uB2E8\uC21C\
  \uD788 \uC778\uB371\uC2A4\uC5D0 \uC758\uD574 \uC904 \uC9C0\uC5B4 \uC788\uB294 \uC22B\
  \uC790\uAC00 \uC544\uB2C8\uB77C, \uC6D0\uD558\uB294 \uC5B4\uB5A4 \uAC83\uC774\uB4E0\
  \ \uD0A4\uB85C \uC0AC\uC6A9\uD560 \uC218 \uC788\uC5B4 \uB370\uC774\uD130 \uAC80\uC0C9\
  \uC774 \uB9E4\uC6B0 \uC27D\uC2B5\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC774 \uC65C \uC774\uAC83\uC744 \uC0AC\uC6A9\uD558\uB098\uC694? \uB54C\uB54C\uB85C\
  , \uB370\uC774\uD130\uB97C \uC904 \uBC88\uD638\uAC00 \uC544\uB2CC \uC774\uB984\uC73C\
  \uB85C \uD638\uCD9C\uD574\uC57C \uD560\u2026"
lastmod: 2024-02-19 22:05:14.319452
model: gpt-4-0125-preview
summary: "Lua\uC5D0\uC11C \uC5F0\uAD00 \uBC30\uC5F4\uC740 \uB9C8\uCE58 \uB370\uC774\
  \uD130\uC758 \uBE44\uBC00 \uC545\uC218\uC640 \uAC19\uC2B5\uB2C8\uB2E4. \uB2E8\uC21C\
  \uD788 \uC778\uB371\uC2A4\uC5D0 \uC758\uD574 \uC904 \uC9C0\uC5B4 \uC788\uB294 \uC22B\
  \uC790\uAC00 \uC544\uB2C8\uB77C, \uC6D0\uD558\uB294 \uC5B4\uB5A4 \uAC83\uC774\uB4E0\
  \ \uD0A4\uB85C \uC0AC\uC6A9\uD560 \uC218 \uC788\uC5B4 \uB370\uC774\uD130 \uAC80\uC0C9\
  \uC774 \uB9E4\uC6B0 \uC27D\uC2B5\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC774 \uC65C \uC774\uAC83\uC744 \uC0AC\uC6A9\uD558\uB098\uC694? \uB54C\uB54C\uB85C\
  , \uB370\uC774\uD130\uB97C \uC904 \uBC88\uD638\uAC00 \uC544\uB2CC \uC774\uB984\uC73C\
  \uB85C \uD638\uCD9C\uD574\uC57C \uD560\u2026"
title: "\uC5F0\uAD00 \uBC30\uC5F4 \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

Lua에서 연관 배열은 마치 데이터의 비밀 악수와 같습니다. 단순히 인덱스에 의해 줄 지어 있는 숫자가 아니라, 원하는 어떤 것이든 키로 사용할 수 있어 데이터 검색이 매우 쉽습니다. 프로그래머들이 왜 이것을 사용하나요? 때때로, 데이터를 줄 번호가 아닌 이름으로 호출해야 할 필요가 있기 때문입니다.

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
