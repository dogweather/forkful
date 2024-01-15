---
title:                "표준 에러에 쓰는 방법"
html_title:           "TypeScript: 표준 에러에 쓰는 방법"
simple_title:         "표준 에러에 쓰는 방법"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

표준 오류를 기록하는 것에 대해 생각한 적이 있나요? 이렇게 하면 디버깅을 쉽게 할 수 있고, 애플리케이션의 오류를 파악하는 데 도움이 됩니다. 또한, 간단한 메시지를 표시하여 사용자에게 문제를 알리는 데도 유용합니다.

## 방법

```TypeScript
console.error("이것은 표준 에러입니다.");
```

위의 예시 코드는 간단하게 표준 오류에 메시지를 출력하는 방법을 보여줍니다. 이 메시지는 콘솔창에서 빨간색으로 표시됩니다. 이렇게 하면 오류가 발생한 위치를 파악하고 해결하기 쉽습니다.

```TypeScript
try {
    // 오류가 발생할 수 있는 코드
} catch (err) {
    console.error(err); // 발생한 오류를 표준 오류에 출력
    // 오류 처리 로직
}
```

위의 예시 코드는 `try-catch` 문을 이용하여 오류가 발생하면 표준 오류에 오류 내용을 출력하는 방법을 보여줍니다. 이렇게 함으로써 애플리케이션의 오류를 파악하고, 사용자에게 알릴 수 있습니다.

## 딥 다이브

표준 오류를 처리하는 또 다른 방법으로는 `process.stderr` 객체를 사용하는 방법이 있습니다. 이 객체를 이용하면 더욱 세부적인 조작이 가능합니다. 또한, `console.error` 메소드와는 달리 오류 객체를 직접 전달할 수 있어 더 유연한 오류 처리가 가능합니다.

```TypeScript
process.stderr.write("이것은 표준 에러입니다.", (err) => {
    if (err) {
        console.error(err);
    }
    // 메시지 출력 후 로직
});
```

위의 예시 코드는 `process.stderr` 객체의 `write` 메소드를 이용하여 오류 메시지를 출력하는 방법을 보여줍니다. 또한, 콜백 함수를 이용하여 메시지가 출력된 후 로직을 수행할 수 있습니다.

## See Also

- [TypeScript 공식 문서](https://www.typescriptlang.org/docs/)
- [Node.js 공식 문서](https://nodejs.org/en/docs/)
- [JavaScript 에러 처리 방법](https://www.javascripttutorial.net/javascript-error-handling/)