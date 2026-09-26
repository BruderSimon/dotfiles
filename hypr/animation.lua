-------------
-- Presets --
-------------
-- Pick one: "none", "mist", "glide", "calm", "blackhole", "orbit", "warp"
local preset = "blackhole"

-- Slowly rotating gradient border (applies to "none"; the presets set their own)
hl.animation({ leaf = "borderangle", enabled = true, speed = 60, bezier = "linear", style = "loop" })

hl.curve("quartOut",{ type = "bezier", points = { {0.25, 1},    {0.5, 1} } })
hl.curve("sine",    { type = "bezier", points = { {0.37, 0},    {0.63, 1} } })        -- gentle in-out
hl.curve("calm",    { type = "spring", mass = 1, stiffness = 200, dampening = 30 })   -- dampening >= 2*sqrt(stiffness*mass): no bounce
hl.curve("expoOut", { type = "bezier", points = { {0.16, 1},    {0.3, 1} } })         -- explosive start, long tail
hl.curve("expoIn",  { type = "bezier", points = { {0.7, 0},     {0.84, 0} } })        -- slow start, runaway finish
hl.curve("quintInOut",{ type = "bezier", points = { {0.83, 0},  {0.17, 1} } })        -- charge up, streak, brake
hl.curve("capture", { type = "spring", mass = 1.2, stiffness = 150, dampening = 16 }) -- slight overshoot, like orbital capture

if preset == "mist" then
    -- pure opacity, nothing moves except when you move it
    hl.animation({ leaf = "borderangle",  enabled = false })
    hl.animation({ leaf = "windows",      enabled = true, speed = 4, bezier = "quartOut" })
    hl.animation({ leaf = "windowsIn",    enabled = true, speed = 4, bezier = "quartOut", style = "popin 100%" })
    hl.animation({ leaf = "windowsOut",   enabled = true, speed = 3, bezier = "sine",     style = "popin 100%" })
    hl.animation({ leaf = "fade",         enabled = true, speed = 4, bezier = "sine" })
    hl.animation({ leaf = "workspaces",   enabled = true, speed = 4, bezier = "sine",     style = "fade" })
    hl.animation({ leaf = "workspacesIn", enabled = true, speed = 4, bezier = "sine",     style = "fade" })
    hl.animation({ leaf = "workspacesOut",enabled = true, speed = 4, bezier = "sine",     style = "fade" })

elseif preset == "glide" then
    -- workspaces glide sideways like pages, windows settle in softly
    hl.animation({ leaf = "borderangle",  enabled = false })
    hl.animation({ leaf = "windows",      enabled = true, speed = 4, bezier = "quartOut" })
    hl.animation({ leaf = "windowsIn",    enabled = true, speed = 4, bezier = "quartOut", style = "slide bottom" })
    hl.animation({ leaf = "windowsOut",   enabled = true, speed = 3, bezier = "sine",     style = "popin 90%" })
    hl.animation({ leaf = "workspaces",   enabled = true, speed = 6, bezier = "quartOut", style = "slide" })
    hl.animation({ leaf = "workspacesIn", enabled = true, speed = 6, bezier = "quartOut", style = "slide" })
    hl.animation({ leaf = "workspacesOut",enabled = true, speed = 6, bezier = "quartOut", style = "slide" })

elseif preset == "calm" then
    -- physical but composed: springs with zero overshoot, border drifts very slowly
    hl.animation({ leaf = "borderangle",  enabled = true, speed = 100, bezier = "linear", style = "loop" })
    hl.animation({ leaf = "windows",      enabled = true, speed = 5, spring = "calm" })
    hl.animation({ leaf = "windowsIn",    enabled = true, speed = 5, spring = "calm", style = "popin 90%" })
    hl.animation({ leaf = "windowsOut",   enabled = true, speed = 3, bezier = "sine", style = "popin 90%" })
    hl.animation({ leaf = "workspaces",   enabled = true, speed = 5, spring = "calm", style = "slidefadevert 20%" })
    hl.animation({ leaf = "workspacesIn", enabled = true, speed = 5, spring = "calm", style = "slidefadevert 20%" })
    hl.animation({ leaf = "workspacesOut",enabled = true, speed = 5, spring = "calm", style = "slidefadevert 20%" })
-- Astrophysics presets

elseif preset == "blackhole" then
    -- windows are born in a big bang and die past the event horizon:
    -- open = explode out of a single point, close = accelerate into it (spaghettification)
    hl.animation({ leaf = "borderangle",  enabled = false })
    hl.animation({ leaf = "windows",      enabled = true, speed = 5, bezier = "expoOut" })
    hl.animation({ leaf = "windowsIn",    enabled = true, speed = 6, bezier = "expoOut", style = "popin 1%" })
    hl.animation({ leaf = "windowsOut",   enabled = true, speed = 4, bezier = "expoIn",  style = "popin 1%" })
    hl.animation({ leaf = "fadeOut",      enabled = true, speed = 4, bezier = "expoIn" })
    hl.animation({ leaf = "workspaces",   enabled = true, speed = 5, bezier = "expoOut", style = "slidefade 10%" })
    hl.animation({ leaf = "workspacesIn", enabled = true, speed = 5, bezier = "expoOut", style = "slidefade 10%" })
    hl.animation({ leaf = "workspacesOut",enabled = true, speed = 5, bezier = "expoIn",  style = "slidefade 10%" })
    hl.animation({ leaf = "specialWorkspaceOut", enabled = true, speed = 5, bezier = "expoIn", style = "slidefade 10%" })

elseif preset == "orbit" then
    -- Kepler: slow at the far point, fast at the close pass. The border gradient orbits the window
    hl.animation({ leaf = "borderangle",  enabled = true, speed = 30, bezier = "linear", style = "loop" })
    hl.animation({ leaf = "windows",      enabled = true, speed = 6, bezier = "sine" })
    hl.animation({ leaf = "windowsIn",    enabled = true, speed = 6, spring = "capture", style = "popin 80%" })
    hl.animation({ leaf = "windowsOut",   enabled = true, speed = 4, bezier = "sine",    style = "popin 80%" })
    hl.animation({ leaf = "windowsMove",  enabled = true, speed = 6, spring = "capture" })
    hl.animation({ leaf = "workspaces",   enabled = true, speed = 7, bezier = "sine",    style = "slidefadevert 30%" })
    hl.animation({ leaf = "workspacesIn", enabled = true, speed = 7, bezier = "sine",    style = "slidefadevert 30%" })
    hl.animation({ leaf = "workspacesOut",enabled = true, speed = 7, bezier = "sine",    style = "slidefadevert 30%" })

elseif preset == "warp" then
    -- hyperspace jump: workspaces charge up, streak across, drop out. Border spins like a pulsar
    hl.animation({ leaf = "borderangle",  enabled = true, speed = 5, bezier = "linear", style = "loop" })
    hl.animation({ leaf = "windows",      enabled = true, speed = 4, bezier = "quintInOut" })
    hl.animation({ leaf = "windowsIn",    enabled = true, speed = 4, bezier = "expoOut",    style = "slide right" })
    hl.animation({ leaf = "windowsOut",   enabled = true, speed = 3, bezier = "expoIn",     style = "slide left" })
    hl.animation({ leaf = "workspaces",   enabled = true, speed = 6, bezier = "quintInOut", style = "slide" })
    hl.animation({ leaf = "workspacesIn", enabled = true, speed = 6, bezier = "quintInOut", style = "slide" })
    hl.animation({ leaf = "workspacesOut",enabled = true, speed = 6, bezier = "quintInOut", style = "slide" })
end
