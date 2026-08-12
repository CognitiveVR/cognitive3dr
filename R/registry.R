# Registry lifecycle state
#
# The canonical field registry lives in cvr-cortex
# (features/slicer/slicer_fields.yaml). It marks two orthogonal facts:
#
#   deprecated: consumers should stop reading the key (a replacement is
#               usually named in the entry description). Says nothing about
#               production — several deprecated keys are still emitted.
#   sunsetted:  no shipping SDK emits the key. Historical data may still be
#               queryable.
#
# This package has no schema-sync machinery (unlike cognitive3dpy, which
# generates its copy), so the lists below are maintained by hand. Keep them
# in sync with the registry; test-compact-columns.R cross-checks them against
# the YAML whenever a cvr-cortex checkout sits alongside this repo.
#
# Last synced against slicer_fields.yaml: 2026-08-12

# Raw registry keys marked "deprecated: true".
deprecated_registry_keys <- c(
  "c3d.device.eyetracking.type",
  "c3d.height",
  "c3d.metric_components.forward_reach_score",
  "c3d.metric_components.fps_data_point_count",
  "c3d.metric_components.fps_data_point_sum",
  "c3d.metric_components.horizontal_reach_score",
  "c3d.metric_components.pitch_score",
  "c3d.metric_components.roll_score",
  "c3d.metric_components.vertical_reach_score",
  "c3d.metrics.app_performance",
  "c3d.metrics.battery_efficiency",
  "c3d.metrics.boundary_score",
  "c3d.metrics.controller_engagement_score",
  "c3d.metrics.controller_ergonomic_score",
  "c3d.metrics.controller_events_score",
  "c3d.metrics.dynamic_engagement_score",
  "c3d.metrics.ergonomics_score",
  "c3d.metrics.head_orientation_score",
  "c3d.metrics.immersion_score",
  "c3d.metrics.orientation_score",
  "c3d.metrics.standing_percentage",
  "c3d.participant.hmdHeight",
  "c3d.roomsize"
)

# Raw registry keys marked "sunsetted: true".
sunsetted_registry_keys <- c(
  "c3d.app.androidPlugin.hostName",
  "c3d.app.androidPlugin.networkHostName",
  "c3d.app.multiplayer.lobbyId",
  "c3d.app.plugin.version",
  "c3d.device.manufacturer",
  "c3d.device.screenresolution",
  "c3d.device.serial_number",
  "c3d.device.serialnumber",
  "c3d.headphonespresent",
  "c3d.height",
  "c3d.oculusId",
  "c3d.participant.Age",
  "c3d.participant.Color",
  "c3d.participant.Job",
  "c3d.participant.Sex",
  "c3d.participant.hmdHeight",
  "c3d.roomscale",
  "cvr.device.graphics.memory",
  "cvr.device.graphics.version",
  "cvr.device.platform",
  "cvr.vr.display.family",
  "cvr.vr.display.model",
  "cvr.vr.enabled"
)

# The same keys as cleaned column names, matching what the parsing pipeline
# produces.
deprecated_registry_columns <- function() {
  vapply(deprecated_registry_keys, clean_property_name, character(1),
         USE.NAMES = FALSE)
}

sunsetted_registry_columns <- function() {
  vapply(sunsetted_registry_keys, clean_property_name, character(1),
         USE.NAMES = FALSE)
}
