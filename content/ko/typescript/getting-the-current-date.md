---
title:    "TypeScript: 현재 날짜 가져오기"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 왜

모바일 앱이나 웹 사이트를 사용하다보면 현재 날짜가 자주 필요하게 됩니다. 이러한 경우에는 TypeScript를 사용하여 현재 날짜를 가져와야 합니다.

# 어떻게

```TypeScript
const now = new Date();
const date = now.getDate();
const month = now.getMonth() + 1;
const year = now.getFullYear();

console.log(`오늘은 ${year}년 ${month}월 ${date}일입니다.`);
```

**출력:**

```
오늘은 2021년 8월 29일입니다.
```

위의 코드에서 `new Date()`를 사용하여 현재 날짜를 가져올 수 있습니다. `getDate()`, `getMonth()`, `getFullYear()`를 이용하여 날짜, 월, 년도를 각각 가져올 수 있습니다. 또한, 월은 0부터 시작하기 때문에 실제 월에 1을 더해주어야 합니다.

# 딥 다이브

`new Date()`는 사용자의 지역 설정에서 가져온 현재 날짜를 반환합니다. 따라서 다른 지역에서 실행할 경우 다른 결과를 얻게 됩니다. 또한, `getTime()`을 이용하면 현재 날짜를 밀리초 단위로 얻을 수 있으며, `getDay()`를 이용하면 요일을 숫자로 얻을 수 있습니다.

# 같이 보기

- [Date 객체](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date/)
- [TypeScript 문서](https://www.typescriptlang.org/docs/)