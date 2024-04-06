---
date: 2024-01-20 18:03:59.102188-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C:) \uC0C8\uB85C\uC6B4 PHP \uD504\uB85C\uC81D\
  \uD2B8\uB97C \uC2DC\uC791\uD558\uB294 \uAC83\uC740 \uAC04\uB2E8\uD574 \uBCF4\uC77C\
  \ \uC218 \uC788\uC9C0\uB9CC, \uC88B\uC740 \uCD08\uC11D\uC744 \uB193\uB294 \uAC83\
  \uC774 \uC911\uC694\uD569\uB2C8\uB2E4. \uACFC\uAC70\uC5D0\uB294 \uBAA8\uB4E0 \uAC83\
  \uC744 \uC218\uB3D9\uC73C\uB85C \uC124\uC815\uD574\uC57C \uD588\uC9C0\uB9CC, \uC624\
  \uB298\uB0A0\uC5D0\uB294 Composer\uC640 \uAC19\uC740 \uB3C4\uAD6C\uB97C \uC0AC\uC6A9\
  \uD558\uC5EC \uC758\uC874\uC131 \uAD00\uB9AC\uB97C \uD558\uACE0, PSR \uD45C\uC900\
  \uC5D0 \uB9DE\uCDB0 \uCF54\uB4DC\uB97C\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.058179-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C:) \uC0C8\uB85C\uC6B4 PHP \uD504\uB85C\uC81D\uD2B8\uB97C\
  \ \uC2DC\uC791\uD558\uB294 \uAC83\uC740 \uAC04\uB2E8\uD574 \uBCF4\uC77C \uC218 \uC788\
  \uC9C0\uB9CC, \uC88B\uC740 \uCD08\uC11D\uC744 \uB193\uB294 \uAC83\uC774 \uC911\uC694\
  \uD569\uB2C8\uB2E4."
title: "\uC0C8 \uD504\uB85C\uC81D\uD2B8 \uC2DC\uC791\uD558\uAE30"
weight: 1
---

## How to: (어떻게:)
```PHP
<?php
// PHP 프로젝트 시작하기
// 프로젝트 폴더 생성
mkdir("my_new_project");

// 기본 index.php 파일 생성
file_put_contents("my_new_project/index.php", "<?php\n// 여기서부터 코드를 작성하세요.\n");

echo "새 프로젝트가 성공적으로 생성되었습니다.";

// Sample output: 새 프로젝트가 성공적으로 생성되었습니다.
```

## Deep Dive (깊이 알아보기)
새로운 PHP 프로젝트를 시작하는 것은 간단해 보일 수 있지만, 좋은 초석을 놓는 것이 중요합니다. 과거에는 모든 것을 수동으로 설정해야 했지만, 오늘날에는 Composer와 같은 도구를 사용하여 의존성 관리를 하고, PSR 표준에 맞춰 코드를 작성합니다. 프로젝트의 성격에 따라 단순히 파일을 만드는 것 이상의 준비가 필요할 수 있습니다. 예를 들어, MVC 구조를 사용할 계획이라면, 이에 맞는 디렉토리 구조를 세심하게 고려해야 합니다.

## See Also (관련 자료)
- PHP 공식 매뉴얼: https://www.php.net/manual/en/
- Composer 홈페이지: https://getcomposer.org/
- PHP-FIG(PSR 표준): https://www.php-fig.org/psr/ 

이러한 자료들은 새 프로젝트를 시작할 때 유용한 정보와 통찰력을 제공합니다.
