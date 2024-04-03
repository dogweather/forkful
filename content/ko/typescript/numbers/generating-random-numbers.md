---
date: 2024-01-27 20:35:50.697435-07:00
description: "TypeScript\uC5D0\uC11C \uB09C\uC218\uB97C \uC0DD\uC131\uD558\uB294 \uAC83\
  \uC740 \uC9C0\uC815\uB41C \uBC94\uC704 \uB0B4\uC5D0\uC11C \uC608\uCE21\uD560 \uC218\
  \ \uC5C6\uB294 \uC22B\uC790 \uAC12\uC744 \uB9CC\uB4DC\uB294 \uAC83\uC5D0 \uB300\uD55C\
  \ \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB7EC\
  \uD55C \uBB34\uC791\uC704 \uC22B\uC790\uB97C \uACE0\uC720 \uC2DD\uBCC4\uC790 \uC0DD\
  \uC131, \uD14C\uC2A4\uD2B8\uB97C \uC704\uD55C \uB370\uC774\uD130 \uC2DC\uBBAC\uB808\
  \uC774\uC158, \uAC8C\uC784 \uBC0F \uC2DC\uBBAC\uB808\uC774\uC158\uC5D0 \uC608\uCE21\
  \ \uBD88\uAC00\uB2A5\uC131 \uCD94\uAC00\uC640 \uAC19\uC740 \uB2E4\uC591\uD55C \uBAA9\
  \uC801\uC73C\uB85C\u2026"
lastmod: '2024-03-13T22:44:54.847226-06:00'
model: gpt-4-0125-preview
summary: "TypeScript\uC5D0\uC11C \uB09C\uC218\uB97C \uC0DD\uC131\uD558\uB294 \uAC83\
  \uC740 \uC9C0\uC815\uB41C \uBC94\uC704 \uB0B4\uC5D0\uC11C \uC608\uCE21\uD560 \uC218\
  \ \uC5C6\uB294 \uC22B\uC790 \uAC12\uC744 \uB9CC\uB4DC\uB294 \uAC83\uC5D0 \uB300\uD55C\
  \ \uAC83\uC785\uB2C8\uB2E4."
title: "\uB09C\uC218 \uC0DD\uC131"
weight: 12
---

## 방법:
TypeScript에서는 전역 `Math` 객체를 사용하여 난수를 생성할 수 있습니다. 아래는 다양한 요구 사항에 대한 난수를 생성하는 방법을 보여주는 실용적인 예시들입니다.

### 기본 난수 생성하기
0(포함)과 1(미포함) 사이의 기본 무작위 소수를 생성하려면, `Math.random()`을 사용합니다. 이는 추가적인 조작을 요구하지 않습니다:

```TypeScript
const randomNumber = Math.random();
console.log(randomNumber);
```

이는 `0.8995452185604771`과 같은 값을 출력할 수 있습니다.

### 두 값 사이의 난수 정수 생성하기
특정 두 값 사이의 정수가 필요할 때는 `Math.random()`과 일부 산술을 포함시킵니다:

```TypeScript
function getRandomInt(min: number, max: number): number {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

const randomInt = getRandomInt(1, 10);
console.log(randomInt);
```

이는 1과 10 사이의 정수 값을 출력할 수 있으며, 예를 들어 `7`과 같습니다.

### 고유 식별자 생성하기
난수는 다른 방법들과 결합되어 고유 식별자를 생성할 수 있으며, 예를 들면 간단한 UUID 생성기 스니펫입니다:

```TypeScript
function generateUUID(): string {
    return 'xxxxyxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
        const r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
        return v.toString(16);
    });
}

const uuid = generateUUID();
console.log(uuid);
```

이는 `110e8400-e29b-41d4-a716-446655440000`과 같은 UUID를 닮은 문자열을 생성합니다.

## 깊이 있게 보기
JavaScript 및 따라서 TypeScript에서 난수를 생성하는 주요 방법인 `Math.random()`은 의사 난수 생성기(PRNG)에 의존합니다. 결과가 무작위처럼 보일 수 있지만, 초기 시드 값에 기반한 결정론적 알고리즘에 의해 생성된다는 점을 유념하는 것이 중요합니다. 따라서, `Math.random()`에 의해 생성된 숫자들은 진정으로 무작위가 아니며, 암호화 목적으로 사용되어서는 안 됩니다.

암호학적으로 안전한 난수를 위해, Web Crypto API는 웹 크립토 표준을 지원하는 환경, 포함한 현대 브라우저와 Node.js( `crypto` 모듈을 통해)에서 접근 가능한 `crypto.getRandomValues()`를 제공합니다. 여기 TypeScript에서 범위 내에서 안전한 난수를 생성하는 용도로 사용되는 간단한 예가 있습니다:

```TypeScript
function secureRandom(min: number, max: number): number {
    const array = new Uint32Array(1);
    window.crypto.getRandomValues(array);
    return min + (array[0] % (max - min + 1));
}

const secureRandNum = secureRandom(1, 100);
console.log(secureRandNum);
```

이 방법은 더 강력한 난수성을 제공하며 보안에 민감한 애플리케이션에 더 적합합니다. 그러나 또한 더 많은 자원을 소모하며 간단한 시뮬레이션 또는 비판적이지 않은 난수 값 생성과 같은 덜 중요한 작업에는 필요하지 않을 수 있습니다.
