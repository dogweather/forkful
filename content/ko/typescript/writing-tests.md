---
title:    "TypeScript: 테스트 작성하기"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## 왜?
프로그래머들은 종종 자신들의 코드를 테스트하는 것에 대한 필요성을 느끼게 됩니다. 테스트는 우리가 작성한 코드가 예상대로 작동하는지 확인하는 중요한 방법입니다. 또한 코드를 수정하거나 추가할 때마다 예기치 않은 버그를 방지하는 데에도 도움이 됩니다.

## 하기전에
타입스크립트로 테스트를 작성하려면 먼저 다음과 같은 두 가지의 패키지가 필요합니다.

1. [Jest](https://jestjs.io/): 자바스크립트를 위한 테스트 프레임워크입니다. 타입스크립트와도 호환됩니다.
2. [ts-jest](https://www.npmjs.com/package/ts-jest): 타입스크립트를 Jest에서 사용할 수 있도록 도와주는 패키지입니다.

또한 tsconfig.json 파일을 생성하여 타입스크립트 컴파일러 옵션을 설정해야 합니다. Jest와 ts-jest의 공식 문서를 참고하여 설정하는 것을 권장합니다.

## 깊게 들어가보기
Jest를 사용하여 타입스크립트 코드를 테스트하는 방법은 간단합니다. 우선, 테스트할 코드를 작성한 뒤, 테스트 파일을 생성합니다. 그 다음, `describe` 함수를 사용하여 테스트 그룹을 만들고, `test` 함수를 사용하여 각각의 테스트를 작성합니다.

예시 코드:

```TypeScript
describe("Calculator", () => {
  test("Addition", () => {
    expect(1 + 1).toBe(2);
  });

  test("Subtraction", () => {
    expect(3 - 2).toBe(1);
  });
});
```

위 코드는 두 개의 테스트를 가진 Calculator 클래스를 테스트하는 예시입니다. `expect` 함수는 예상되는 결과와 실제 결과가 일치하는지를 확인합니다. `toBe` 함수는 값을 일치시키는 용도로 사용됩니다.

## 참고
- [Jest 공식 문서](https://jestjs.io/docs/getting-started)
- [ts-jest Github 저장소](https://github.com/kulshekhar/ts-jest)
- [타입스크립트 공식 문서](https://www.typescriptlang.org/docs/)