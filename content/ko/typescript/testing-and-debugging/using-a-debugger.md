---
date: 2024-01-26 04:11:23.665698-07:00
description: "\uB514\uBC84\uAC70\uB294 \uCF54\uB4DC\uAC00 \uC2E4\uD589\uB418\uB294\
  \ \uB3D9\uC548 \uCF54\uB4DC\uC758 \uB0B4\uBD80 \uB3D9\uC791\uC744 \uAC80\uD1A0\uD558\
  \uACE0 \uBCC0\uACBD\uD560 \uC218 \uC788\uAC8C \uD574\uC8FC\uB294 \uB3C4\uAD6C\uC785\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uCF54\uB4DC\uB97C \uB2E8\uACC4\
  \uBCC4\uB85C \uC774\uD589\uD558\uBA74\uC11C \uBCC0\uC218\uB97C \uAC80\uC0AC\uD558\
  \uACE0 \uD504\uB85C\uADF8\uB7A8\uC758 \uD750\uB984\uC744 \uC774\uD574\uD568\uC73C\
  \uB85C\uC368 \uBC84\uADF8\uB97C \uC81C\uAC70\uD558\uB294 \uB370\uC5D0 \uC774\uB97C\
  \ \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
lastmod: '2024-02-25T18:49:51.868772-07:00'
model: gpt-4-0125-preview
summary: "\uB514\uBC84\uAC70\uB294 \uCF54\uB4DC\uAC00 \uC2E4\uD589\uB418\uB294 \uB3D9\
  \uC548 \uCF54\uB4DC\uC758 \uB0B4\uBD80 \uB3D9\uC791\uC744 \uAC80\uD1A0\uD558\uACE0\
  \ \uBCC0\uACBD\uD560 \uC218 \uC788\uAC8C \uD574\uC8FC\uB294 \uB3C4\uAD6C\uC785\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uCF54\uB4DC\uB97C \uB2E8\uACC4\uBCC4\
  \uB85C \uC774\uD589\uD558\uBA74\uC11C \uBCC0\uC218\uB97C \uAC80\uC0AC\uD558\uACE0\
  \ \uD504\uB85C\uADF8\uB7A8\uC758 \uD750\uB984\uC744 \uC774\uD574\uD568\uC73C\uB85C\
  \uC368 \uBC84\uADF8\uB97C \uC81C\uAC70\uD558\uB294 \uB370\uC5D0 \uC774\uB97C \uC0AC\
  \uC6A9\uD569\uB2C8\uB2E4."
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇과 왜?
디버거는 코드가 실행되는 동안 코드의 내부 동작을 검토하고 변경할 수 있게 해주는 도구입니다. 프로그래머는 코드를 단계별로 이행하면서 변수를 검사하고 프로그램의 흐름을 이해함으로써 버그를 제거하는 데에 이를 사용합니다.

## 방법:

TypeScript에서 디버거를 사용하기 위해서는 지원되는 IDE(예: Visual Studio Code)와 `launch.json` 설정만 있으면 됩니다. Node.js 애플리케이션을 위한 간단한 예를 들어보겠습니다:

```TypeScript
// app.ts
function greet(name: string) {
    console.log(`Hello, ${name}!`);
}

const userName = 'Ada';
greet(userName);
```

이것을 디버깅하려면, `.vscode` 폴더 아래에 `launch.json` 파일을 생성합니다:

```JSON
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "node",
            "request": "launch",
            "name": "Launch Program",
            "skipFiles": ["<node_internals>/**"],
            "program": "${workspaceFolder}/app.ts",
            "preLaunchTask": "tsc: build - tsconfig.json",
            "outFiles": ["${workspaceFolder}/build/**/*.js"]
        }
    ]
}
```

그런 다음, IDE에서 줄 번호 왼쪽을 클릭하여 `greet` 함수에 중단점을 설정합니다. F5를 눌러 디버깅을 시작하고, 애플리케이션이 중단점에서 일시 중지되는 것을 관찰하세요. 이제 변수를 마우스로 가리키고, 표현식을 관찰하며, 코드를 쉽게 단계별로 이행할 수 있습니다.

## 심층 탐구

통합 개발 환경(IDE)이 정교해지기 이전에는, 디버깅은 종종 print 문장(일명 `console.log` 디버깅)으로 수행되곤 했습니다. 어느 정도 작동은 했지만, 눈가리개를 쓴 채로 짚단에서 바늘을 찾는 것과 같았습니다.

현대의 디버거는 문제 해결을 위한 스위스 아미 나이프와 같습니다. TypeScript와 Node.js의 발전과 함께 다양한 디버거가 사용 가능해졌는데, 내장된 Node.js 검사기부터 클라이언트 측 디버깅을 위한 브라우저 개발 도구까지 다양합니다.

Node.js 검사기는 실행 중인 애플리케이션에 연결하여 작동하며, Chrome DevTools 프로토콜을 통해 통신함으로써 Chrome 브라우저를 강력한 디버깅 콘솔로 변환합니다. 이 통합은 전통적인 명령줄 디버깅 관행에 비해 시각적으로 상호 작용이 가능하고 상세한 디버깅 세션을 가능하게 합니다.

## 참고자료

조금 더 읽어보고 몇 가지 전문가 팁을 확인해 보세요:

- [Visual Studio Code에서의 TypeScript 디버깅](https://code.visualstudio.com/docs/typescript/typescript-debugging)
- [Node.js 디버깅 가이드](https://nodejs.org/en/docs/guides/debugging-getting-started/)
- [Chrome DevTools 문서](https://developers.google.com/web/tools/chrome-devtools)
