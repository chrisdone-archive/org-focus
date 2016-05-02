# org-focus

Focus on your work with better time tracking management for org-mode.

## Suggested keybindings

For setting estimates and scheduling tasks:

``` lisp
(define-key org-mode-map (kbd "C-c C-e") 'org-focus-estimate)
(define-key org-mode-map (kbd "C-c C-s") 'org-focus-schedule)
```

Something to view your agenda:

``` lisp
(define-key org-mode-map (kbd "C-x C-c") 'org-focus)
```

## Description

It adds an extra field to the SCHEDULED property in org-mode:

```
* TODO Meeting with Mary
  SCHEDULED: <2016-05-01 Sun> <1 hour>
```

You schedule an item in for a day and then set a plan of how long
you expect to spend on it.

We add an extra property specific to this workflow called ESTIMATE,
which pertains the the task overall.

```
* TODO Meeting with Mary
  ESTIMATE: <2 hours>
  SCHEDULED: <2016-05-01 Sun> <1 hour>
  SCHEDULED: <2016-05-02 Mon> <1 hour>
```

This allows us to have a task that spans many days, with an overall
estimate. Then each day that you're working on the task, you can
put in an amount of time you plan to spend on it (either hopeful or
a pestimate, as you prefer).
