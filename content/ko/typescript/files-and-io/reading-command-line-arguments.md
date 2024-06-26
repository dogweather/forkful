---
date: 2024-01-20 17:56:56.473029-07:00
description: "How to: TypeScript\uC5D0\uC11C \uBA85\uB839\uC904 \uC778\uC218\uB97C\
  \ \uC77D\uC73C\uB824\uBA74 `process.argv`\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4.\
  \ \uAC04\uB2E8\uD55C \uC608\uC81C\uB97C \uC0B4\uD3B4\uBD05\uC2DC\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:54.875207-06:00'
model: gpt-4-1106-preview
summary: "TypeScript\uC5D0\uC11C \uBA85\uB839\uC904 \uC778\uC218\uB97C \uC77D\uC73C\
  \uB824\uBA74 `process.argv`\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30"
weight: 23
---

## How to:
TypeScript에서 명령줄 인수를 읽으려면 `process.argv`를 사용합니다. 간단한 예제를 살펴봅시다.

```typescript
// args.ts
const args = process.argv.slice(2);

console.log(args);
```

이 스크립트를 다음과 같이 실행하면:
```bash
ts-node args.ts one two "three four"
```

출력은 이렇게 나옵니다:
```bash
[ 'one', 'two', 'three four' ]
```

## Deep Dive
명령줄 인수를 읽는 것은 거의 모든 프로그래밍 언어에서 지원하는 기본적인 기능입니다. 초기 프로그래밍 시대부터 사용자와 상호작용할 방법으로 터미널 또는 명령 프롬프트를 사용했습니다. TypeScript는 Node.js 환경 위에서 작동하므로 `process.argv` 배열을 통해 접근합니다. 이 배열의 처음 두 요소는 실행경로와 파일경로를 갖습니다, 그래서 흔히 `slice(2)`를 사용해 실제 인수만을 추립니다.

다른 방법으로는 `yargs`나 `commander` 와 같은 명령줄 인수 파싱 라이브러리도 있습니다. 이 라이브러리들은 복잡한 인수 및 옵션 구조를 관리하는 데 유용합니다.

`process.argv`의 장점은 별도의 라이브러리나 의존성 없이 바로 사용할 수 있다는 것입니다. 다만, 복잡한 명령줄 어플리케이션을 구축할 때는 라이브러리를 고려해볼 수 있습니다.

## See Also
- Node.js `process.argv` documentation: https://nodejs.org/docs/latest/api/process.html#process_process_argv
- `yargs` library: https://www.npmjs.com/package/yargs
- `commander` library: https://www.npmjs.com/package/commander
