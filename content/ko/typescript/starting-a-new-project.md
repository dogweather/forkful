---
title:    "TypeScript: 새 프로젝트 시작하기"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 왜

새 프로젝트를 시작하는 이유는 다양합니다. 새로운 기술을 익히고 자신의 스킬을 발전시키기 위해서일 수도 있고, 새로운 아이디어를 구현하고 이를 공유하기 위해서일 수도 있습니다. 어떤 이유로든 자신의 프로그래밍 능력을 발전시키고 싶다면 새 프로젝트를 시작하는 것을 추천합니다.

## 어떻게 시작하나요?

새 TypeScript 프로젝트를 시작하려면 Node.js와 npm이 설치된 컴퓨터가 필요합니다. 먼저 `npm init`을 사용하여 프로젝트 디렉토리를 초기화하고, `npm install -g typescript` 명령어를 사용하여 TypeScript를 전역으로 설치합니다. 그리고 나서 다음과 같이 아주 기초적인 예제 코드를 작성해보겠습니다.

```TypeScript
// main.ts 파일
const greeting: string = "안녕하세요!";
console.log(greeting); // 출력: 안녕하세요!
```

이제 `tsc main.ts` 명령어로 TypeScript 파일을 컴파일하고, `node main.js` 명령어를 사용하여 실행하면 "안녕하세요!"가 콘솔에 출력됩니다.

## 깊게 보기

새로운 TypeScript 프로젝트를 시작하는 것은 자유롭게 발전시킬 수 있는 큰 가능성을 가지고 있습니다. 더 많은 기능을 추가하고 다양한 라이브러리를 사용해보고 싶다면 많은 문서를 참조하고 다른 개발자들의 코드를 참고하는 것이 좋습니다. 또한 자신의 프로젝트를 개선하기 위해 검색할 수 있는 다양한 도구들도 있으니 적극 활용해보세요.

---

# 참고 자료

- [타입스크립트 공식 문서](https://www.typescriptlang.org/docs/)
- [MDN 웹 문서](https://developer.mozilla.org/ko/)
- [TypeScript 기초 강의](https://www.inflearn.com/course/%ED%83%80%EC%9E%85%EC%8A%A4%ED%81%AC%EB%A6%BD%ED%8A%B8-%EA%B8%B0%EC%B4%88) 
- [TypeScript 코딩 스타일 가이드](https://basarat.gitbooks.io/typescript/docs/styleguide/styleguide.html)