---
date: 2024-01-20 17:50:50.782053-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Fish Shell\uC5D0\uC11C\
  \ \uBB38\uC790\uC5F4 \uBCF4\uAC04\uC744 \uC0AC\uC6A9\uD558\uB294 \uAC83\uC740 \uB2E8\
  \uC21C\uD569\uB2C8\uB2E4. \uBCC0\uC218 \uAC12\uC774\uB098 \uBA85\uB839\uC5B4 \uCD9C\
  \uB825\uC744 \uBB38\uC790\uC5F4\uACFC \uACB0\uD569\uD558\uB824\uBA74 $ \uAE30\uD638\
  \uB97C \uC0AC\uC6A9\uD558\uC138\uC694. \uC544\uB798 \uC608\uC2DC\uB97C \uCC38\uC870\
  \uD558\uC138\uC694."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.833006-06:00'
model: gpt-4-1106-preview
summary: "Fish Shell\uC5D0\uC11C \uBB38\uC790\uC5F4 \uBCF4\uAC04\uC744 \uC0AC\uC6A9\
  \uD558\uB294 \uAC83\uC740 \uB2E8\uC21C\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
weight: 8
---

## How to: (어떻게 하나요?)
Fish Shell에서 문자열 보간을 사용하는 것은 단순합니다. 변수 값이나 명령어 출력을 문자열과 결합하려면 $ 기호를 사용하세요. 아래 예시를 참조하세요.

```Fish Shell
set name "세상"
echo "안녕, $name!" # 변수 보간
# 출력: 안녕, 세상!

set greeting (echo "안녕")
echo "$greeting, 다시 만났군요!" # 명령어 치환
# 출력: 안녕, 다시 만났군요!
```

## Deep Dive (심층 탐구)
Fish Shell에서의 문자열 보간은 타 스크립팅 언어에서도 흔히 볼 수 있는 기능입니다. 예를 들어, Bash에서는 `$`를 이용하거나 `${}`로 변수를 감싸서 사용합니다. Fish는 따로 감싸지 않고 바로 사용하는 간결함이 특징입니다.

과거에 명령어 치환은 백틱(`)으로 이루어졌지만, 이는 가독성이 떨어지고 중첩 사용에서 불편함이 있어서 Fish와 다른 현대 쉘들은 $(...) 형식을 사용합니다. 

Fish Shell의 구현을 살펴보면, 문자열 보간은 명령어 치환과 변수 치환 모두 `eval` 함수 없이 내부적으로 처리되어, 실수로 코드를 실행하는 일 없이 보안을 강화합니다.

## See Also (더 보기)
- 공식 Fish Shell 문서: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Fish Shell GitHub Repository: [https://github.com/fish-shell/fish-shell](https://github.com/fish-shell/fish-shell)
- 문자열 보간에 대한 더 깊은 토론: [https://fishshell.com/docs/current/tutorial.html#tut_quoting](https://fishshell.com/docs/current/tutorial.html#tut_quoting)
