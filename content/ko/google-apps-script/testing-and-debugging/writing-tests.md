---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:10:10.787021-07:00
description: "\uBC29\uBC95: Google Apps \uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uB294 \uB2E4\
  \uB978 \uD504\uB85C\uADF8\uB798\uBC0D \uD658\uACBD\uCC98\uB7FC \uB0B4\uC7A5 \uD14C\
  \uC2A4\uD305 \uD504\uB808\uC784\uC6CC\uD06C\uAC00 \uC5C6\uC9C0\uB9CC, \uB2E8\uC21C\
  \uD55C GAS \uD568\uC218\uB97C \uD65C\uC6A9\uD558\uAC70\uB098 `QUnit`\uACFC \uAC19\
  \uC740 \uC678\uBD80 \uD14C\uC2A4\uD305 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uD1B5\
  \uD569\uD558\uC5EC \uC5EC\uC804\uD788 \uD14C\uC2A4\uD2B8\uB97C \uC791\uC131\uD558\
  \uACE0 \uC2E4\uD589\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC5EC\uAE30 \uC2A4\uD06C\
  \uB9BD\uD2B8 \uB0B4 \uB2E4\uB978 \uD568\uC218\uB97C \uD14C\uC2A4\uD2B8\uD558\uAE30\
  \u2026"
lastmod: '2024-03-13T22:44:54.536199-06:00'
model: gpt-4-0125-preview
summary: "Google Apps \uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uB294 \uB2E4\uB978 \uD504\uB85C\
  \uADF8\uB798\uBC0D \uD658\uACBD\uCC98\uB7FC \uB0B4\uC7A5 \uD14C\uC2A4\uD305 \uD504\
  \uB808\uC784\uC6CC\uD06C\uAC00 \uC5C6\uC9C0\uB9CC, \uB2E8\uC21C\uD55C GAS \uD568\
  \uC218\uB97C \uD65C\uC6A9\uD558\uAC70\uB098 `QUnit`\uACFC \uAC19\uC740 \uC678\uBD80\
  \ \uD14C\uC2A4\uD305 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uD1B5\uD569\uD558\uC5EC\
  \ \uC5EC\uC804\uD788 \uD14C\uC2A4\uD2B8\uB97C \uC791\uC131\uD558\uACE0 \uC2E4\uD589\
  \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uD14C\uC2A4\uD2B8 \uC791\uC131\uD558\uAE30"
weight: 36
---

## 방법:
Google Apps 스크립트에는 다른 프로그래밍 환경처럼 내장 테스팅 프레임워크가 없지만, 단순한 GAS 함수를 활용하거나 `QUnit`과 같은 외부 테스팅 라이브러리를 통합하여 여전히 테스트를 작성하고 실행할 수 있습니다. 여기 스크립트 내 다른 함수를 테스트하기 위해 단순한 GAS 함수를 사용한 기본적인 예시가 있습니다:

```javascript
function add(a, b) {
  return a + b;
}

function testAdd() {
  var result = add(2, 3);
  if (result !== 5) {
    throw new Error("테스트 실패: add(2, 3)는 5여야 하는데, 결과는 " + result + "입니다.");
  } else {
    Logger.log("테스트 통과!");
  }
}
```

`testAdd()`를 실행하면 `add` 함수가 올바르게 작동하면 "테스트 통과!"를 로그로 남기고, 그렇지 않으면 오류를 발생시킵니다. 보다 정교한 접근 방식으로, Google Apps 스크립트와 QUnit을 통합하는 것은 몇 단계가 더 필요하지만, 강력한 테스팅 환경을 제공합니다. QUnit 테스트 설정의 샘플은 다음과 같습니다:

1. 프로젝트에 QUnit 라이브러리를 포함시킵니다.
2. QUnit 테스트를 실행하기 위한 테스트 HTML 파일을 생성합니다.
3. QUnit의 문법을 사용하여 테스트 케이스를 작성합니다.

QUnit을 사용한 예시는 다음과 같습니다:

```javascript
// 테스트를 실행하는 HTML 파일에 링크함으로써 QUnit을 포함하세요

QUnit.test("add 함수 테스팅", function (assert) {
  var result = add(2, 3);
  assert.equal(result, 5, "add(2, 3)는 5를 반환해야 합니다.");
});
```

결과를 보려면 GAS 스크립트 편집기 내에서 HTML 파일을 열거나 웹 앱으로 배포하세요.

## 심층 분석
역사적으로, Google Apps 스크립트에서의 테스팅은 플랫폼의 기원과 기본 사용 사례가 대규모 애플리케이션보다는 빠르고 소규모 자동화 작업에 중점을 두어 어느 정도 간과되었습니다. 그 결과, GAS는 보다 전통적인 프로그래밍 환경에서 찾아볼 수 있는 강력한 테스팅 프레임워크와 도구를 제공하지 않습니다. 하지만, 커뮤니티는 오픈 소스 라이브러리를 도입하고 Google의 기존 도구를 창의적으로 활용함으로써 적응했습니다.

QUnit과 같은 라이브러리를 사용하는 것은 큰 진전을 의미하지만, 적합한 테스팅 환경을 설정하고 추가 문법을 배우는 등의 도전이 수반됩니다. 그러나, GAS로 더 복잡하고 신뢰할 수 있는 애플리케이션을 구축하는 데 투자하는 이들에게는 그 노력이 가치가 있습니다.

간단한 GAS 함수를 사용한 테스팅과 같은 대안은 추가적인 의존성 없이 GAS 환경과의 통합 및 사용 편의성을 제공하지만, 프로젝트가 성장함에 따라 쉽게 확장할 수 없으며 포괄적인 테스팅 기능을 결여하고 있습니다. clasp( Google Apps 스크립트 명령줄 인터페이스)와 같은 도구는 개발자가 선호하는 IDE에서 코딩을 할 수 있게 하여, 외부 테스팅 프레임워크와 더 원활하게 통합할 수 있는 고급 워크플로우를 포함한 테스팅을 용이하게 합니다.

결론적으로, GAS가 상자에서 나오자마자 고급 테스팅을 기본적으로 지원하지 않을 수 있지만, 그 유연성과 커뮤니티의 혁신적인 접근 방식은 스크립트가 견고하고 신뢰할 수 있으며 어떤 작업에도 대비할 준비가 되도록 하는 실행 가능한 경로를 제공합니다.
