---
date: 2024-01-25 02:54:33.067718-07:00
description: "Kicking off a new PHP project means setting up a foundation to build\
  \ your web app or script. We do it to start fresh, structure our ideas into code,\
  \ and\u2026"
lastmod: '2024-03-13T22:45:00.167364-06:00'
model: gpt-4-1106-preview
summary: "Kicking off a new PHP project means setting up a foundation to build your\
  \ web app or script. We do it to start fresh, structure our ideas into code, and\u2026"
title: Starting a new project
---

{{< edit_this_page >}}

## What & Why?
Kicking off a new PHP project means setting up a foundation to build your web app or script. We do it to start fresh, structure our ideas into code, and solve new problems.

## How to:
Kickstart your project by choosing a structure. Composer's your friend here. Run this:

```bash
composer init
```

Then, create your `index.php`. It's your entry point:

```php
<?php
// index.php
echo "Welcome to my new PHP project!";
```

Test it on your local server. You should see:

```
Welcome to my new PHP project!
```

## Deep Dive
Back in the day, PHP projects began with simple scripts. No package management, no frameworks, just pure PHP files. Now, we’ve got Composer, a tool to manage dependencies, autoload classes, and set up autoloading standards like PSR-4. It’s standard practice for modern PHP projects.

You could roll old-school, no Composer, no autoloaders. But why ignore the convenience and standards widely adopted?

Use frameworks like Laravel or Symfony for complex apps. They provide structure and tools, speeding up development. For smaller projects, micro-frameworks like Slim might be enough.

Implementation wise, consider environment variables for config, adopt PSR standards for coding style and structure, and don't skip on a version control system like Git.

## See Also
- [Composer](https://getcomposer.org/)
- [PHP The Right Way](https://phptherightway.com/)
- [PSR Standards](https://www.php-fig.org/psr/)
- [Laravel](https://laravel.com/)
- [Symfony](https://symfony.com/)
- [Slim Framework](http://www.slimframework.com/)
