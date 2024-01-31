---
title:                "현재 날짜 가져오기"
date:                  2024-01-20T15:17:05.795419-07:00
html_title:           "Bash: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"

category:             "TypeScript"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜?)
현재 날짜를 얻는 것은 단순히 오늘의 날짜와 시간을 파악하는 것입니다. 프로그래머들은 로깅, 타임스탬프, 기능적 기간 제한 설정 등을 위해 이를 사용합니다.

## How to: (방법)
```TypeScript
// 현재 날짜와 시간을 얻는 방법
const now: Date = new Date();
console.log(now);
// 예시 출력: 2023-04-02T15:23:31.123Z (출력값은 실행할 때마다 다릅니다)

// 현재 날짜만 얻는 방법 (시간 정보는 무시)
const today: Date = new Date();
today.setHours(0, 0, 0, 0);
console.log(today);
// 예시 출력: 2023-04-02T00:00:00.000Z (출력값은 실행할 때마다 다릅니다)
```

## Deep Dive (심도 있는 분석)
JavaScript에서 날짜와 시간은 `Date` 객체로 다루어진다. 이는 1970년 1월 1일 00:00:00 UTC로부터 경과한 밀리초를 기반으로 한다. 더 많은 방법은 `moment.js` 같은 라이브러리를 사용할 수 있지만, `Date` 객체를 사용하는 것이 TypeScript를 포함한 모든 JavaScript 환경에서 기본적이고, 직접적인 방법이다.

## See Also (참조)
- MDN Web Docs의 `Date` 객체: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- `moment.js` 라이브러리: https://momentjs.com/
- TypeScript Handbook: https://www.typescriptlang.org/docs/handbook/intro.html
