---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:15.212221-07:00
description: "TypeScript\uC5D0\uC11C, JavaScript \uC704\uC5D0 \uAD6C\uCD95\uB41C \uC5B8\
  \uC5B4\uC5D0\uC11C, \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC00\uC838\uC624\uB294 \uAC83\
  \uC740 \uD604\uC7AC \uB0A0\uC9DC\uC640 \uC2DC\uAC04 \uC815\uBCF4\uC5D0 \uC811\uADFC\
  \uD558\uC5EC \uC870\uC791\uD560 \uC218 \uC788\uAC8C \uD569\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uC885\uC885 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\
  \uC5D0\uC11C \uD0C0\uC784\uC2A4\uD0EC\uD504 \uC0DD\uC131, \uC2A4\uCF00\uC904\uB9C1\
  , \uADF8 \uBC16\uC758 \uC2DC\uAC04\uC5D0 \uBBFC\uAC10\uD55C \uAE30\uB2A5\uC744 \uB9CC\
  \uB4E4\uAE30 \uC704\uD574 \uC774 \uAE30\uB2A5\uC744\u2026"
lastmod: '2024-02-25T18:49:51.878247-07:00'
model: gpt-4-0125-preview
summary: "TypeScript\uC5D0\uC11C, JavaScript \uC704\uC5D0 \uAD6C\uCD95\uB41C \uC5B8\
  \uC5B4\uC5D0\uC11C, \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC00\uC838\uC624\uB294 \uAC83\
  \uC740 \uD604\uC7AC \uB0A0\uC9DC\uC640 \uC2DC\uAC04 \uC815\uBCF4\uC5D0 \uC811\uADFC\
  \uD558\uC5EC \uC870\uC791\uD560 \uC218 \uC788\uAC8C \uD569\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uC885\uC885 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\
  \uC5D0\uC11C \uD0C0\uC784\uC2A4\uD0EC\uD504 \uC0DD\uC131, \uC2A4\uCF00\uC904\uB9C1\
  , \uADF8 \uBC16\uC758 \uC2DC\uAC04\uC5D0 \uBBFC\uAC10\uD55C \uAE30\uB2A5\uC744 \uB9CC\
  \uB4E4\uAE30 \uC704\uD574 \uC774 \uAE30\uB2A5\uC744\u2026"
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
TypeScript에서, JavaScript 위에 구축된 언어에서, 현재 날짜를 가져오는 것은 현재 날짜와 시간 정보에 접근하여 조작할 수 있게 합니다. 프로그래머들은 종종 애플리케이션에서 타임스탬프 생성, 스케줄링, 그 밖의 시간에 민감한 기능을 만들기 위해 이 기능을 필요로 합니다.

## 어떻게:
TypeScript에서는 `Date` 객체를 사용하여 현재 날짜와 시간을 얻을 수 있습니다. 다음은 그 방법입니다:

```typescript
const currentDate = new Date();
console.log(currentDate);
```

샘플 출력:
```
2023-04-12T07:20:50.52Z
```

이 코드 스니펫은 현재 날짜와 시간을 포함하는 새로운 `Date` 객체를 생성하고, 그것을 콘솔에 출력합니다. 또한 toLocaleDateString()을 사용하여 날짜를 더 읽기 쉬운 형식으로 포맷할 수 있습니다:

```typescript
const currentDate = new Date();
console.log(currentDate.toLocaleDateString());
```

샘플 출력:
```
4/12/2023
```

### date-fns 사용하기
더 광범위한 날짜 조작 및 형식 지정을 위해, `date-fns` 라이브러리는 인기 있는 선택입니다. 먼저 npm을 통해 설치하세요:

```bash
npm install date-fns
```

그런 다음, 현재 날짜를 형식화하는 데 사용할 수 있습니다:

```typescript
import { format } from 'date-fns';

const currentDate = new Date();
console.log(format(currentDate, 'yyyy-MM-dd'));
```

샘플 출력:
```
2023-04-12
```

이 `date-fns` 예제는 현재 날짜를 "YYYY-MM-DD" 형식의 문자열로 포맷합니다. 라이브러리는 날짜 조작을 위한 다양한 기능을 제공하여, 날짜 작업을 하는 모든 TypeScript 프로그래머에게 다재다능한 도구가 됩니다.
