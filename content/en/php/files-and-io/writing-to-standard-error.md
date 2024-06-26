---
date: 2024-02-03 19:03:36.286422-07:00
description: "How to: In PHP, writing to stderr can be achieved using the `fwrite()`\
  \ function alongside the predefined constant `STDERR`, which represents the error\u2026"
lastmod: '2024-03-13T22:45:00.181954-06:00'
model: gpt-4-0125-preview
summary: In PHP, writing to stderr can be achieved using the `fwrite()` function alongside
  the predefined constant `STDERR`, which represents the error output stream.
title: Writing to standard error
weight: 25
---

## How to:
In PHP, writing to stderr can be achieved using the `fwrite()` function alongside the predefined constant `STDERR`, which represents the error output stream.

```php
<?php
// Writing a simple message to stderr.
fwrite(STDERR, "This is an error message.\n");
```

Sample output when the script is executed from the command line:
```
This is an error message.
```

To demonstrate more practical usage, consider a scenario where you're parsing user input and encounter unexpected data:
```php
<?php
$input = 'unexpected data';

// Simulating an error processing user input.
if ($input === 'unexpected data') {
    fwrite(STDERR, "Error: Unexpected input received.\n");
    exit(1); // Exiting with a non-zero value to indicate an error.
}
```

While PHP's built-in capabilities for handling stderr are generally sufficient, when dealing with more complex applications or wanting to integrate stderr logging with external systems, third-party libraries like Monolog can be a powerful ally. Monolog is a logging library that can handle stderr among many other targets (files, sockets, etc.).

Using Monolog to write to stderr:

First, ensure you have Monolog installed via Composer:
```
composer require monolog/monolog
```

Then, you can configure Monolog to use the `StreamHandler` targeted at `php://stderr`:

```php
<?php
require 'vendor/autoload.php';

use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// Create a log channel
$log = new Logger('name');
$log->pushHandler(new StreamHandler('php://stderr', Logger::WARNING));

// Add a log message to stderr
$log->warning('This is a warning message.');
```

The above code utilizes Monolog to send a warning message to stderr, which is particularly useful for applications that require detailed logging configurations or external log monitoring.
